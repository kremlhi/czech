;;; emms-player-vlc-ext.el --- extending vlc support for EMMS

;; Copyright (C) 2008, 2009 Free Software Foundation, Inc.

;; Authors: Yoni Rabkin <yonirabkin@member.fsf.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'emms-player-vlc)

(setq emms-player-vlc-parameters '("--control=rc" "--fullscreen"))

;; enable process buffer to read available subtitles
(defadvice emms-player-vlc-start (around quit-vlc-after-finish activate)
  (let ((process (apply 'start-process
                        emms-player-simple-process-name
                        emms-player-simple-process-name
                        emms-player-vlc-command-name
                        ;; splice in params here
                        (append emms-player-vlc-parameters
                                (list (emms-track-name (ad-get-arg 0)))
                                '("vlc://quit")))))
    ;; add a sentinel for signaling termination
    (set-process-sentinel process 'emms-player-simple-sentinel))
  (emms-player-started emms-player-vlc))

;; improve speed/accuracy in (re)winding
(defun emms-player-vlc-seek2 (sec)
  "Seek relative within a stream."
  (process-send-string
   emms-player-simple-process-name
   (format "seek %s%d\n" (if (< 0 sec) "+" "") sec)))

(defun emms-player-vlc-current-sub (lines)
  (mapcar (lambda (x)
            (if (string-match "\\([0-9][0-9]*\\) - \\(.*\\) \\*$" x)
                (let ((idx (string-to-number (match-string 1 x)))
                      (title (match-string 2 x)))
                  (list idx (- (length lines) 3) title))))
          lines))

(defun emms-player-vlc-get-subtitle ()
  (let* ((proc emms-player-simple-process-name)
          (oldp (with-current-buffer proc (point))))
    (process-send-string proc "strack\n")
    (accept-process-output (get-process proc) 0 100)
    (let* ((input (with-current-buffer proc (buffer-substring oldp (point))))
           (lines (split-string input "\r\n")))
      (car (remove-if #'null (emms-player-vlc-current-sub lines))))))

(defun emms-player-vlc-prev-subtitle ()
  (interactive)
  (message (emms-player-vlc-change-subtitle -1)))

(defun emms-player-vlc-next-subtitle ()
  (interactive)
  (message (emms-player-vlc-change-subtitle 1)))
    
(defun emms-player-vlc-change-subtitle (n)
  (let* ((proc emms-player-simple-process-name)
         (sub (emms-player-vlc-get-subtitle)))
    (cond ((null sub) "none")
          (t (let* ((idx (car sub))
                    (tot (cadr sub))
                    (next (+ (mod idx tot) n)))
               (process-send-string proc (format "strack %s\n" next))
               (caddr (emms-player-vlc-get-subtitle)))))))

(emms-player-set emms-player-vlc 'seek 'emms-player-vlc-seek2)
(emms-player-set emms-player-vlc 'get-sub 'emms-player-vlc-get-subtitle)
(emms-player-set emms-player-vlc 'prev-sub 'emms-player-vlc-prev-subtitle)
(emms-player-set emms-player-vlc 'next-sub 'emms-player-vlc-next-subtitle)

;; > strack
;; +----[ spu-es ]
;; | -1 - Disable
;; | 2 - Track 1 *
;; +----[ end of spu-es ]
;; > strack 1
;; > strack
;; +----[ spu-es ]
;; | -1 - Disable *
;; | 2 - Track 1
;; +----[ end of spu-es ]

(provide 'emms-player-vlc-ext)

;;; emms-player-vlc-ext.el ends here
