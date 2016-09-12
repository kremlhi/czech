(require 'emms-dir-mode)
(require 'distel)
(eval-when-compile
  (require 'cl))

(defvar czech-player-active nil)
(defvar czech-ebin-dir (expand-file-name
                        (concat (file-name-directory load-file-name)
                                "../ebin")))
(defvar czech-erlang-node nil)

(defun czech-start-hook (node _fsm)
  (setq czech-erlang-node node)
  (czech-start))

(defun czech-setup ()
  (distel-setup)
  (add-hook 'erl-nodeup-hook 'czech-start-hook))

(defun czech-read-node ()
  (or czech-erlang-node
      (setq czech-erlang-node (erl-target-node))))

(defun czech-start ()
  (add-hook 'emms-player-started-hook 'czech-set-player-active)
  (add-hook 'emms-player-stopped-hook 'czech-set-player-inactive) ;user interaction
  (add-hook 'emms-player-finished-hook 'czech-set-player-inactive)
  (erl-spawn
    (let ((node (czech-read-node)))
      (setq erl-trap-exit t)
      (call-interactively 'erl-ping node)
      (czech-add-code-path node))))

(defun czech-add-code-path (node)
  (message "add load path %s to %s" czech-ebin-dir node)
  (erl-send-rpc node 'code 'add_patha (list czech-ebin-dir))
  (erl-receive (node)
      ((['rex 'true] t)
       (['rex resp]
        (error "unexpected response: %s" resp)))
    (&czech-start-app node)))

(defun &czech-start-app (node)
  (message "start app czech")
  (erl-send-rpc node 'czech_app 'start ())
  (erl-receive (node)
      ((['rex 'ok] t)
       (['rex resp]
        (error "unexpected response: %s" resp)))
    (&czech-subscribe node)))

(defun &czech-subscribe (node)
  (message "subscribe %s" node)
  (erl-send-rpc node 'czech 'subscribe (list erl-self))
  (erl-receive (node)
      ((['rex ['ok from]]
        (erl-dist-link from)
        (message "subscribe done %s" from))
       (['rex resp]
        (error "unexpected response: %s" resp)))
    (&czech-loop node)))

(defun &czech-loop (node)
  (erl-receive (node)
      ((['keypress from key]
        (ignore-errors ;don't care about stuff throwing errors
          (czech-handle-keypress key)))
       (['keyrel from]
        (czech-handle-keyrel))
       (['volume from mute vol]
        (czech-handle-volume mute vol))
       (['EXIT from reason]
        (message "oshit %s died: %s @%s" from reason node)
        (sit-for 2)
        (&czech-subscribe node))
       (other (message "cecmsg %S" other)))
    (&czech-loop node)))

(defun czech-handle-keyrel () t)

(defun czech-handle-volume (mute vol)
  (message "volume %s%%%s" vol
           (if (eq mute 'true) " [mute]" "")))

;; czech ! {self(),{cec,{[],0,0,68,[<<68>>]}}}.
(defun czech-handle-keypress (key)
  (with-selected-window (selected-window)
    (unless (buffer-live-p emms-playlist-buffer)
      (call-interactively #'emms-dir))
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

(provide 'czech-start)

;;; czech-start.el ends here
