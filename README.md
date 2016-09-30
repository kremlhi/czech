EMMS + distel + p8 =:= love

Sit comfortable on your sofa and let Emacs play your favourite movies.

Installation instructions:
--------------------------

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

(setq inferior-erlang-machine-options '("-sname" "emacs"))

(require 'czech-start)
(czech-setup)

M-x load-file <your init file>

On a Mac, make sure Emms can start VLC.app from a vlc command in $PATH,
or fiddle with:

(setq emms-player-vlc-command-name "/Applications/VLC.app/Contents/MacOS/VLC")

M-x erlang-shell
M-x erl-ping emacs

If this doesn't trigger the erl-node-hook in elisp/czech-start.el then:

M-x czech-start

How stuff works?
----------------

    +---[ Emacs ]---+   +--[ BEAM ]--+   +-[ p8adpt ]-+   +-[ TV ]-+
    | czech.el      |---| distel     |---|            |---|        |
    | emms-dir-mode |   | czech, p8  |   |            |   |        |
    +---------------+   +------------+   +------------+   +--------+

When (czech-start) is invoked (from erl-node-up-hook) in
elisp/czech-start.el an erlang node is started from Emacs with the
src/czech_app.erl application loaded. The czech application starts a
supervisor with a general purpose HDMI CEC gen_server (src/czech.erl)
and a HW interface (src/p8.erl). p8 starts a C program,
c_src/p8adpt.c. p8adpt connects to pulse-eight USB CEC adapter via USB
and communicates with it as a serial device.

Whenever a key is pressed on a users' remote control, signals are sent
from the TV over the HDMI CEC bus. The signals are received in
c_src/p8adpt.c and parsed in src/p8.erl and src/p8_packet.erl.

There are three types of signals (defined in src/p8.hrl):
1. commands to control the p8 adapter (i.e. ping, get_firmware_vsn, etc)
2. CEC indications received from devices over the HDMI CEC bus
3. CEC commands originated from czech

The key press event is sent as an erlang message with the following
format:

    {cec,Flags,Src,Dest,Op,Params}
      when Flags :: [{idle,T} | {timeout,T} | {ack_p,B}],
           T :: 0..255, B :: 0 | 1,
           Src :: 0..15,
           Dest :: 0..15,
           Op :: 0..255,
           Params :: binary().

from p8 to czech. In czech the event is sent to czech.el (again as an
erlang message):

    {keypress,From,Key}
      when From :: pid(),
           Key :: atom() | integer().

When the message arrives in czech the Key is translated into an Emacs
command, in a buffer with Emms dir mode.

[p8]: https://www.pulse-eight.com/ "P8 USB CEC adapter"
[cecomatic]: http://www.cec-o-matic.com/ "my obscure HDMI-CEC frame"
[hdmi13a]: http://www.microprocessor.org/HDMISpecification13a.pdf
* 8.6 Consumer Electronics Control (CEC)
* Supplement 1 - Consumer Electronics Control (CEC)

TODO:
-----
* be able to go to / in emms-dir-mode
* make sttl button work (will probably require ?CEC_VENDOR_COMMANDs)
* put a '*' in emms-player-started-hook
* add abnormal hooks *-functions to czech-start.el
* say hello to TV if p8 is active and wake up display
* negative test cases (send #ind_err{})

For Mac haters:
---------------
* elisp/czech.el: use of AppleScript to switch between VLC and Emacs
* src/czech.erl: caffeinate(1) to wake up sleepy Macs
* src/p8.erl: p8 USB device hardcoded to /dev/cu.usbmodemv2_r1
