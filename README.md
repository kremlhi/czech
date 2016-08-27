EMMS + distel + p8 =:= love

Sit comfortable on your sofa and let Emacs play your favourite movies.

Installation instructions:

$ make

Put this stuff in your ~/.emacs.d/init.el or ~/.emacs:

(defun erl-root ()
  (or (getenv "OTP_ROOT")
      (shell-command-to-string
       "erl -noinput -eval 'io:format(\"~s\",[code:root_dir()]),halt().'")))

(defun cond-load-distel ()
  (let ((distel (concat <PATH TO DISTEL> "/distel/elisp"))
        (czech (concat <PATH TO CZECH> "/czech/elisp")))
    (when (file-exists-p distel)
      (add-to-list 'load-path distel)
      (require 'distel)
      (distel-setup)
      (when (file-exists-p czech)
        (add-to-list 'load-path czech)
        (require 'czech)
        (load "czech")))))

(defun set-erlang-dir (dir)
  (let ((bin-dir (expand-file-name "bin" dir))
        (tools-dirs (file-expand-wildcards
                     (concat dir "/lib/tools-*/emacs"))))
    (when tools-dirs
      (add-to-list 'load-path (car tools-dirs))
      (add-to-list 'exec-path bin-dir)
      (setq erlang-root-dir dir)
      (setq erl-nodename-cache (intern (concat "emacs@" (system-name))))
      (setq inferior-erlang-machine-options
            (list "-name" (symbol-name erl-nodename-cache)))
      (require 'erlang-start)
      (cond-load-distel)))))
(set-erlang-dir (erl-root))

(defun czech-start-hook (node _fsm)
  (setq czech-erlang-node node)
  (czech-start))
(add-hook 'erl-nodeup-hook 'czech-start-hook)

M-x load-file <your init file>

TODO:
* say hello to TV if p8 is active and wake up display
* negative test cases (send #ind_err{})

For Mac haters:
* czech.el: use of AppleScript to switch between VLC and Emacs
* czech.erl: caffeinate(1) to wake up sleepy Macs
* p8.erl: p8 USB device hardcoded to /dev/cu.usbmodemv2_r1
