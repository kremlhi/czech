(require 'erl-service)
(eval-when-compile (require 'cl))

(require 'emms-setup)
(emms-all)
(emms-default-players)

(require 'emms-player-vlc)

(defun czech-start ()
  (emms)
  (erl-spawn
    ;;(setq erl-trap-exit t)
       (erl-send-rpc (erl-target-node) 'czech 'subscribe (list erl-self))
    ;;    (erl-send (tuple 'czech (erl-target-node)) 'tja)
;;    (erl-register 'emms)
    (erl-receive ()
        ((['rex ['badrpc reason]]
          (message "Bad RPC: %s" reason))
         (['rex result]
          (progn (message "result %s" result)
                 (czech-loop)))))))

(defun czech-loop ()
  (erl-receive ()
      ((['keypress pid key]
        (ignore-errors
          (czech-handle-keypress key)))
       (['keyrel pid]
        (czech-handle-keyrel))
       (['volume pid mute vol]
        (czech-handle-volume mute vol))
       (other (message "cecmsg %S" other)))
    (czech-loop)))

(defun czech-handle-keyrel () t)

(defun czech-handle-volume (mute vol)
  (message "volume %s%%%s" vol
           (if (eq mute 'true) " [mute]"
             "")))

(defun czech-handle-keypress (key)
  (with-selected-window (selected-window)
    (cond ((eq key 'enter)
           (progn
             (setq czech-player-active t)
             (emms-playlist-mode-play-smart)))
          ((eq key 'up) (forward-line -1))
          ((eq key 'down) (forward-line))
          ((eq key 'left) (emms-seek -10))
          ((eq key 'right) (emms-seek 10))
          ((eq key 'cancel) (czech-alt-tab))
          ((eq key 'ch_up) (forward-line -20))
          ((eq key 'ch_down) (forward-line 20))
          ((eq key 'volup) t)
          ((eq key 'voldown) t)
          ((eq key 'pause) (emms-pause))
          ((eq key 'stop)
           (progn
             (setq czech-player-active nil)
             (emms-stop)))
          ((eq key 'play)
           (progn
             (setq czech-player-active t)
             (emms-pause)))
          ((eq key 'rew) (emms-seek -90))
          ((eq key 'ff) (emms-seek 90))
          ((eq key 'skip_next) (emms-next))
          ((eq key 'skip_prev) (emms-previous))
          ((eq key 'd0) (delete-other-windows))
          ((eq key 'd8) (message (format "%S" (current-buffer))))
          ((eq key 'd9) (emms-playlist-mode-goto-dired-at-point))
          (t (message "key %s" key)))))

(defun ns-raise-vlc ()
  "Raise VLC."
  (ns-do-applescript "tell application \"VLC\" to activate"))

(defun ns-raise-emacs ()
  "Raise Emacs."
  (ns-do-applescript "tell application \"Emacs\" to activate"))

(defun ns-raise-emacs-with-frame (frame)
  "Raise Emacs and select the provided frame."
  (with-selected-frame frame
    (when (display-graphic-p)
      (ns-raise-emacs))))

(defvar czech-player-active t)
(defun czech-alt-tab ()
  (setq czech-player-active (not czech-player-active))
  (if czech-player-active (ns-raise-vlc)
     (progn (ns-raise-emacs) (emms))
    czech-player-active))


;; (setq emms-source-file-default-directory "~/Music/")
;; 'M-x emms-add-directory-tree RET ~/Music/ RET'.

;; modifications needed to enable emms to control VLC

;; emms-setup-default-player-list is a variable defined in
;; `emms-setup.el'.  Its value is (emms-player-mpg321
;; emms-player-ogg123 emms-player-mplayer-playlist emms-player-mplayer
;; emms-player-vlc emms-player-vlc-playlist)

;; (define-emms-simple-player vlc '(file url)
;;   (concat "\\`\\(http[s]?\\|mms\\)://\\|"
;;   (apply #'emms-player-simple-regexp
;;  emms-player-base-format-list))
;;   "vlc" "--control=rc" "--fullscreen")

;; (define-emms-simple-player vlc-playlist '(streamlist)
;;   "\\`http[s]?://"
;;   "vlc" "--control=rc" "--fullscreen")
