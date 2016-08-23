(require 'emms-dir-mode)
(require 'erl-service)
(eval-when-compile
  (require 'cl))

;; TODO: move this crap

;; (defun start1 ()
;;   (erl-spawn
;;     (setq erl-trap-exit t)
;;     (rereg)
;;     (loop1 0)))

;; (defun rereg ()
;;   (erl-send-rpc (erl-target-node) 'czech_el 'add_handler (list erl-self)))

;; (defun loop1 (cnt)
;;   (message "loop1 %s" cnt)
;;   (erl-receive (cnt)
;;       ((['EXIT pid rsn] (rereg))
;;        (other (message "loop1 %s" other)))
;;     (loop1 (1+ cnt))))


;; (erl-send-rpc (erl-target-node) 'io 'format '("hello you~n"))

;; (erl-spawn
;;   (erl-send [TYPE erl-pid distel_1048@skoll\.local 16 0 0] 'hejsan))

;; (erl-spawn
;;   (erl-send ['TYPE erl-pid erl-node-name 54 0 0] 'hejsan))

(defun czech-start ()
  (call-interactively #'emms-dir)
  (add-hook 'emms-player-started-hook 'czech-set-player-active)
  (add-hook 'emms-player-stopped-hook 'czech-set-player-inactive) ;user interaction
  (add-hook 'emms-player-finished-hook 'czech-set-player-inactive)

  (erl-spawn
    (erl-send-rpc (erl-target-node) 'el_proxy 'add_handler (list erl-self))
    ;;(erl-send (tuple 'czech (erl-target-node)) 'tja)
    ;; (erl-register 'emms)
    (erl-receive ()
        ((['rex ['badrpc reason]]
          (message "Bad RPC: %s" reason))
         (['rex result]
          (progn (message "result %s" result)
                 (czech-loop)))))))

(defun czech-loop ()
  (erl-receive ()
      ((['keypress pid key]
        (ignore-errors ;don't care about stuff throwing errors
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
           (if (eq mute 'true) " [mute]" "")))

;; czech ! {self(),{cec,{[],0,0,68,[<<68>>]}}}.
;; el_proxy ! {keypress,self(),play}.
(defun czech-handle-keypress (key)
  (with-selected-window (selected-window)
    (unless (eq (current-buffer) emms-playlist-buffer)
      (switch-to-buffer emms-playlist-buffer))
    (cond ((eq key 'enter) (funcall (local-key-binding "\C-m")))
          ((eq key 'up) (funcall (local-key-binding "\C-p")))
          ((eq key 'down) (funcall (local-key-binding "\C-n")))
          ((eq key 'left) (funcall (local-key-binding "<")))
          ((eq key 'right) (funcall (local-key-binding ">")))
          ((eq key 'return) (funcall (local-key-binding "^")))
          ((eq key 'cancel) (czech-alt-tab))
          ((eq key 'ch_up) (funcall (local-key-binding "\M-v")))
          ((eq key 'ch_down) (funcall (local-key-binding "\C-v")))
          ((eq key 'volup) t)
          ((eq key 'voldown) t)
          ((eq key 'pause) (funcall (local-key-binding "P")))
          ((eq key 'stop) (funcall (local-key-binding "s")))
          ((eq key 'play) (funcall (local-key-binding "P")))
          ((eq key 'rew) (funcall (local-key-binding ",")))
          ((eq key 'ff) (funcall (local-key-binding ".")))
          ((eq key 'skip_next) (funcall (local-key-binding "n")))
          ((eq key 'skip_prev) (funcall (local-key-binding "p")))
          ((eq key 'sttl) (funcall (local-key-binding "8")))

          (t (message "key %s" key)))))

(defvar czech-player-active nil)
(defun czech-alt-tab ()
  (setq czech-player-active (not czech-player-active))
  (if czech-player-active
      (ns-raise-vlc)
    (ns-raise-emacs)
    czech-player-active))

(defun czech-set-player-active ()
  (setq czech-player-active t))
(defun czech-set-player-inactive ()
  (setq czech-player-active nil))

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

