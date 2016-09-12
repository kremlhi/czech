EMMS + distel + p8 =:= love

Sit comfortable on your sofa and let Emacs play your favourite movies.

Installation instructions:

$ make

Put this stuff in your ~/.emacs.d/init.el or ~/.emacs:

(defun erl-root ()
  (or (getenv "OTP_ROOT")
      (shell-command-to-string
       "erl -noinput -eval 'io:format(\"~s\",[code:root_dir()]),halt().'")))

(add-to-list
   'load-path
       (car (file-expand-wildcards (concat (erl-root) "/lib/tools-*/emacs"))))
(add-to-list 'load-path (concat (getenv "HOME") "/share/distel/elisp"))
(add-to-list 'load-path (concat (getenv "HOME") "/share/czech/elisp"))

(setq inferior-erlang-machine-options
      (list "-name" (concat "emacs@" (system-name))))

(require 'czech-start)
(czech-setup)

M-x load-file <your init file>

TODO:
* make sttl button work
* put a '*' in emms-player-started-hook
* add abnormal hooks *-functions to czech-start.el
* make it easier to ins
* say hello to TV if p8 is active and wake up display
* negative test cases (send #ind_err{})

For Mac haters:
* czech.el: use of AppleScript to switch between VLC and Emacs
* czech.erl: caffeinate(1) to wake up sleepy Macs
* p8.erl: p8 USB device hardcoded to /dev/cu.usbmodemv2_r1
