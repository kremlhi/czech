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
(eval-when-compile
  (require 'cl))

;; --intf does not work, use --control instead
(setq emms-player-vlc-parameters '("--control=rc" "--fullscreen"))

;; improve speed/accuracy in (re)winding
(defun emms-player-vlc-seek2 (sec)
  "Seek relative within a stream."
  (process-send-string
   emms-player-simple-process-name
   (format "seek %s%d\n" (if (< 0 sec) "+" "") sec)))

(defun emms-player-vlc-next-subtitle ()
  (interactive)
  (let ((p (get-process emms-player-simple-process-name)))
    (set-process-filter p 'emms-player-vlc-next-sub-filter)
    (process-send-string p "strack\n")))

;; bleh, bother to learn *lisp when you have erlang :)
(defun takewhile (c lst)
  (if (funcall c (car lst))
      (cons (car lst) (takewhile c (cdr lst)))
    nil))

(defun dropwhile (c lst)
  (if (funcall c (car lst))
      (dropwhile c (cdr lst))
    lst))

(defun emms-player-vlc-sub-list (string)
  (let* ((subs
          (mapcar (lambda (x)
                    (if (string-match "\\([0-9]+\\) - \\(.*?\\)\\( \\*\\)?$" x)
                        (list (if (match-string 3 x) t)
                              (string-to-number (match-string 1 x))
                              (match-string 2 x))))
                  (split-string string "[\r\n]+"))))
    (emms-player-vlc-sub-sort (remove-if #'null subs))))

;; put the active sub as head of the list
(defun emms-player-vlc-sub-sort (subs)
  (let* ((fn (lambda (x) (not (car x))))
         (fst (dropwhile fn subs))
         (snd (takewhile fn subs)))
    (append fst snd)))

(defun emms-player-vlc-inc-idx (string)
  (cadr (cadr (emms-player-vlc-sub-list string))))

(defun emms-player-vlc-next-sub-filter (p string)
  (let ((idx (emms-player-vlc-inc-idx string)))
    (cond ((null idx)
           (set-process-filter p nil)
           (message "none"))
          (t
           (set-process-filter p 'emms-player-vlc-get-sub-filter)
           (process-send-string p (format "strack %d\nstrack\n" idx))))))

(defun emms-player-vlc-get-sub-filter (p string)
  (set-process-filter p nil)
  (message "%s" (caddr (car (emms-player-vlc-sub-list string)))))

(defun emms-player-vlc-current-sub (lines)
  (mapcar (lambda (x)
            (if (string-match "\\([0-9][0-9]*\\) - \\(.*\\) \\*$" x)
                (let ((idx (string-to-number (match-string 1 x)))
                      (title (match-string 2 x)))
                  (list idx (- (length lines) 3) title))))
          lines))

(emms-player-set emms-player-vlc 'seek 'emms-player-vlc-seek2)
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
