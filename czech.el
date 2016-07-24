(require 'erl-service)
(eval-when-compile (require 'cl))

(require 'emms-setup)
(emms-all)
(emms-default-players)

(require 'emms-player-vlc)

(defun czech-start ()
  (emms)
  (erl-spawn
    (erl-send-rpc (erl-target-node) 'czech 'subscribe (list erl-self))
    (erl-receive ()
        ((['rex ['badrpc reason]]
          (message "Bad RPC: %s" reason))
         (['rex result]
          (progn (message "result %s" result)
                 (client-loop)))))))

(defun czech-loop ()
  (erl-receive ()
      ((['keypress pid key]
        (client-handle-keypress key))
       (['keyrel pid]
        (client-handle-keyrel))
       (['volume pid mute vol]
        (client-handle-volume mute vol))
       (other (message "cecmsg %S" other)))
    (client-loop)))

(defun client-handle-keyrel () t)

(defun client-handle-volume (mute vol)
  (message "volume %s%%%s" vol
           (if (eq mute 'true) " [mute]"
             "")))

(defun client-handle-keypress (key)
  (with-current-buffer " *EMMS Playlist*"
    (cond ((eq key 'enter) (emms-playlist-mode-play-smart))
          ((eq key 'up) (forward-line -1))
          ((eq key 'down) (forward-line))
          ((eq key 'left) (emms-seek -10))
          ((eq key 'right) (emms-seek 10))
          ((eq key 'cancel) (client-alt-tab))
          ((eq key 'ch_up) (forward-line -20))
          ((eq key 'ch_down) (forward-line 20))
          ((eq key 'volup) t)
          ((eq key 'voldown) t)
          ((eq key 'pause) (emms-pause))
          ((eq key 'stop) (emms-stop))
          ((eq key 'play) (emms-pause))
          ((eq key 'rew) (emms-seek -90))
          ((eq key 'ff) (emms-seek 90))
          ((eq key 'skip_next) (emms-next))
          ((eq key 'skip_prev) (emms-previous))
          (t (message "key %s" key)))))

;; (when (featurep 'ns)
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

(defvar client-switch t)
(defun client-alt-tab ()
  (setq client-switch (not client-switch))
  (if client-switch (progn (ns-raise-emacs) (emms))
    (ns-raise-vlc)
    client-switch))

;; (defun client-go ()
;;   (switch-to-buffer emms-playlist-buffer)
;; ;;  (with-current-buffer emms-playlist-buffer
;;     (next-line)
;;     (next-line)
;;     (next-line))


;; (setq emms-source-file-default-directory "~/Music/")
;; 'M-x emms-add-directory-tree RET ~/Music/ RET'.


;; ta bort mplayer

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
