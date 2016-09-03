;; To make this the default EMMS mode, do:
;;   (setq emms-playlist-default-major-mode 'emms-dir-mode)

(require 'emms)
(require 'emms-playlist-mode)
(require 'emms-player-vlc-ext)
(eval-when-compile
  (require 'cl))

(defun emms-dir ()
  (interactive)
  (emms-dir-list (or emms-source-file-default-directory ".")))

(defun emms-dir-cd-or-play ()
  (interactive)
  (let ((line (buffer-substring (+ (point-at-bol) 2) (point-at-eol))))
    (if (file-directory-p line)
        (emms-dir-list line)
      (emms-playlist-mode-play-smart))))

(defun emms-dir-cd-up ()
  (interactive)
  (emms-dir-list ".."))

(define-derived-mode emms-dir-mode emms-playlist-mode "Emms-Dir"
  "A major mode for the Emms dir.
\\{emms-dir-mode-map}"
  (setq-local emms-track-description-function #'emms-dir-track-description)
  (setq-local revert-buffer-function #'emms-dir-revert))

(defun emms-dir-file-media-p (file)
  (member (file-name-extension (downcase file))
          emms-player-base-format-list))

(defun emms-dir-insert-media-files (path)
  (mapc (lambda (x)
          (let ((file (concat (file-name-as-directory path) x)))
            (when (emms-dir-file-media-p file)
              (let ((track (emms-track 'file file)))
                (emms-playlist-mode-insert-track track)))))
        (directory-files path)))

(defun emms-dir-track-description (track)
  (let ((type (emms-track-type track))
        (count (emms-track-get track 'play-count)))
    (concat (if count "* " "  ")
            (cond ((eq 'file type)
                   (file-name-nondirectory (emms-track-name track)))
                  ((eq 'url type)
                   (emms-format-url-track-name (emms-track-name track)))
                  (t (concat (symbol-name type)
                             ": " (emms-track-name track)))))))

(defconst emms-dir-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-n") 'next-line)
    (define-key map (kbd "C-p") 'previous-line)
    (define-key map (kbd "M-<") 'emms-playlist-mode-first)
    (define-key map (kbd "M->") 'emms-playlist-mode-last)
    (define-key map (kbd "M-n") 'emms-playlist-mode-next)
    (define-key map (kbd "M-p") 'emms-playlist-mode-previous)
    (define-key map (kbd "n") 'emms-next)
    (define-key map (kbd "p") 'emms-previous)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd ">") 'emms-seek-forward)
    (define-key map (kbd "<") 'emms-seek-backward)
    (define-key map (kbd ".") 'emms-seek-fforward)
    (define-key map (kbd ",") 'emms-seek-fbackward)
    (define-key map (kbd "8") 'emms-player-vlc-next-subtitle)
    (define-key map (kbd "P") 'emms-pause)
    (define-key map (kbd "s") 'emms-stop)
    (define-key map (kbd "f") 'emms-show)
    (define-key map (kbd "c") 'emms-playlist-mode-center-current)
    (define-key map (kbd "q") 'emms-playlist-mode-bury-buffer)
    (define-key map (kbd "r") 'emms-random)
    (define-key map (kbd "d") 'emms-playlist-mode-goto-dired-at-point)
    (define-key map (kbd "<mouse-2>") 'emms-dir-cd-or-play)
    (define-key map (kbd "RET") 'emms-dir-cd-or-play)
    (define-key map (kbd "^") 'emms-dir-cd-up)
    (define-key map (kbd "g") 'revert-buffer)
    map)
  "Keymap for `emms-dir-mode'.")

(defun emms-dir-insert-playlist (x)
  (insert "  " x)
  (add-text-properties (+ (point-at-bol) 2) (point-at-eol)
                       '(face emms-playlist-track-face))
  (newline))

(defun emms-dir-insert-dirs (path)
  (mapc (lambda (x)
          (when (file-directory-p x)
            (emms-dir-insert-playlist x)))
        (directory-files path)))

(defun emms-dir-filter-buf (path)
  (remove-if (lambda (buf)
               (not (and (buffer-live-p buf)
                         (string= path (with-current-buffer buf
                                         default-directory)))))
             (emms-playlist-buffer-list)))

(defun emms-dir-playlist-buf (path)
  (unless (file-directory-p path)
    (error "Directory not found"))
  (or (car (emms-dir-filter-buf path))
      (emms-playlist-new (file-name-nondirectory path))))

(defun emms-dir-list (path)
  (let* ((epath (expand-file-name path))
         (new (null (emms-dir-filter-buf epath)))
         (buf (emms-dir-playlist-buf epath))
         (pns)
         (inhibit-read-only t))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (setq emms-playlist-buffer buf)
      (setq emms-playlist-buffer-p t)
      (setq default-directory epath)
      (setq pns (emms-dir-save-positions))
      (emms-playlist-clear)
      (emms-dir-insert-stuff epath)
      (unless new
        (emms-dir-restore-positions pns))
      buf)))

(defun emms-dir-insert-stuff (epath)
  (insert "  " epath ":")
  (newline)
  (emms-dir-insert-dirs epath)
  (emms-dir-insert-media-files epath)
  (goto-char (point-min))
  (forward-line 3))

(defun emms-dir-revert (&optional _arg _noconfirm)
  (widen)
  (let ((pns (emms-dir-save-positions))
        (inhibit-read-only t))
    (emms-playlist-clear)
    (emms-dir-insert-stuff default-directory)
    (emms-dir-restore-positions pns)))

(defun emms-dir-save-positions ()
  (mapcar (lambda (w)
            (cons w (window-point w)))
          (get-buffer-window-list nil 0 t)))

(defun emms-dir-restore-positions (positions)
  (dolist (win-file-pos positions)
    (with-selected-window (car win-file-pos)
      (goto-char (cdr win-file-pos)))))

(defun emms-seek-fbackward ()
  (interactive)
  (when emms-player-playing-p
    (emms-player-seek -60)))

(defun emms-seek-fforward ()
  (interactive)
  (when emms-player-playing-p
    (emms-player-seek 60)))

(provide 'emms-dir-mode)

;;; emms-dir-mode.el ends here
