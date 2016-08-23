EMMS + distel + p8 =:= love

Sit comfortable on your sofa and let Emacs play your favourite movies.

0. Patch emms-player-vlc.el (with changes found in czech.el)
1. Start Emacs with distel and EMMS loaded.
2. In the *erlang* buffer run: czech_sup:start_link().
3. M-x load-file czech.el
4. M-: (czech-start)

TODO:
* add erl-nodeup-hook to bring up czech.el (and distel and EMMS)
* a user friendly startup script
* say hello to TV if p8 is active and wake up display
* negative test cases (send #ind_err{})

For Mac haters:
* czech.el: use of AppleScript to switch between VLC and Emacs
* czech.erl: caffeinate(1) to wake up sleepy Macs
* p8.erl: p8 USB device hardcoded to /dev/cu.usbmodemv2_r1
