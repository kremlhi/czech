(require 'erlang)
(require 'distel)
(require 'emms-setup)
(require 'emms-dir-mode)
(eval-when-compile
  (require 'cl))

(defvar czech-player-active nil)
(defvar czech-ebin-dir (expand-file-name
                        (concat (file-name-directory load-file-name)
                                "../ebin")))
(defvar czech-erlang-node nil)
(defvar czech-idle nil)
(defvar czech-idle-timer nil)

(defun czech-start-hook (node _fsm)
  (setq czech-erlang-node node)
  (czech-start))

(defun czech-setup ()
  (distel-setup)
  (emms-standard)
  (emms-default-players)
  (setq emms-setup-default-player-list
      '(emms-player-vlc emms-player-vlc-playlist))
  (setq emms-playlist-default-major-mode 'emms-dir-mode)
  (add-hook 'erl-nodeup-hook 'czech-start-hook)
  (add-hook 'emms-player-started-hook 'czech-set-player-active)
  (add-hook 'emms-player-stopped-hook 'czech-set-player-inactive);user interact
  (add-hook 'emms-player-finished-hook 'czech-set-player-inactive)
  t)

(defun czech-read-node ()
  (or czech-erlang-node
      (setq czech-erlang-node (erl-target-node))))

(defun czech-start ()
  (interactive)
  (erl-spawn
    (let ((node (czech-read-node)))
      (setq erl-trap-exit t)
      (czech-start-idle-timer)
      (czech-handle-activate)
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
          (if czech-idle (czech-handle-activate)
            (czech-handle-keypress key))))
       (['keyrel from]
        (czech-handle-keyrel))
       (['volume from mute vol]
        (czech-handle-volume mute vol))
       (['activate from]
        (czech-handle-activate))
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

;; czech ! {self(),{cec,[],0,0,68,[<<68>>]}}.
(defun czech-handle-keypress (key)
  (with-selected-window (selected-window)
    (unless (buffer-live-p emms-playlist-buffer)
      (call-interactively #'emms-dir))
    (unless (eq (current-buffer) emms-playlist-buffer)
      (switch-to-buffer emms-playlist-buffer))
    (let ((cmd
           (cond ((eq key 'enter) "\C-m")
                 ((eq key 'up) "\C-p")
                 ((eq key 'down) "\C-n")
                 ((eq key 'left) "<")
                 ((eq key 'right) ">")
                 ((eq key 'return) "^")
                 ((eq key 'cancel) (czech-alt-tab) nil)
                 ((eq key 'ch_up) "\M-v")
                 ((eq key 'ch_down) "\C-v")
                 ((eq key 'volup) nil)
                 ((eq key 'voldown) nil)
                 ((eq key 'pause) "P")
                 ((eq key 'stop) "s")
                 ((eq key 'play) "P")
                 ((eq key 'rew) ",")
                 ((eq key 'ff) ".")
                 ((eq key 'skip_next) "n")
                 ((eq key 'skip_prev) "p")
                 ;; ((eq key 'sttl) "8")
                 ((eq key 'd8) "8")
                 (t (message "key %s" key) nil))))
      (when cmd
          (funcall (key-binding cmd))))))

(defun czech-handle-activate ()
  (setq czech-idle nil)
  (shell-command "caffeinate -u -t 1")
  (message "cec device activated"))

(defun czech-alt-tab ()
  (cond ((eq czech-player-active t)
         (ns-raise-emacs)
         (czech-set-player-inactive))
        (t
         (ns-raise-vlc)
         (czech-set-player-active))))

(defun czech-set-player-active ()
  (czech-cancel-idle-timer)
  (setq czech-player-active t))

(defun czech-set-player-inactive ()
  (czech-start-idle-timer)
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

(defun czech-cancel-idle-timer ()
  (when czech-idle-timer
    (cancel-timer czech-idle-timer)
    (setq czech-idle-timer nil)))

(defun czech-start-idle-timer ()
  (czech-cancel-idle-timer)
  (setq czech-idle-timer
        (run-at-time (czech-displaysleep) nil 'czech-set-idle)))

(defun czech-set-idle ()
  (czech-cancel-idle-timer)
  (setq czech-idle-timer
        (run-at-time (czech-displaysleep) nil 'czech-set-idle))
  (setq czech-idle t))

(defun czech-displaysleep ()
  (cond ((locate-file "pmset" exec-path)
         (concat
          (cadr (assoc "displaysleep"
                       (mapcar (lambda (x) (cdr (split-string x " +")))
                               (split-string
                                (shell-command-to-string "pmset -g") "\n"))))
          " minutes"))
        (t "11 minutes")))

(provide 'czech-start)

;;; czech-start.el ends here
