(defvar-local doom-large-file-p nil)
(put 'doom-large-file-p 'permanent-local t)

(defvar doom-large-file-size-alist '(("." . 3.0))
  "An alist mapping regexps (like `auto-mode-alist') to filesize thresholds.

If a file is opened and discovered to be larger than the threshold, Doom
performs emergency optimizations to prevent Emacs from hanging, crashing or
becoming unusably slow.

These thresholds are in MB, and is used by `doom--optimize-for-large-files-a'.")

(defvar doom-large-file-excluded-modes
  '(so-long-mode
    special-mode archive-mode tar-mode jka-compr
    git-commit-mode image-mode doc-view-mode doc-view-mode-maybe
    ebrowse-tree-mode pdf-view-mode tags-table-mode)
  "Major modes that `doom-check-large-file-h' will ignore.")

(defun doom--optimize-for-large-files-a (orig-fn &rest args)
  "Set `doom-large-file-p' if the file is too large.

Uses `doom-large-file-size-alist' to determine when a file is too large. When
`doom-large-file-p' is set, other plugins can detect this and reduce their
runtime costs (or disable themselves) to ensure the buffer is as fast as
possible."
  (if (setq doom-large-file-p
            (and buffer-file-name
                 (not doom-large-file-p)
                 (file-exists-p buffer-file-name)
                 (ignore-errors
                   (> (nth 7 (file-attributes buffer-file-name))
                      (* 1024 1024
                         (assoc-default buffer-file-name
                                        doom-large-file-size-alist
                                        #'string-match-p))))))
      (prog1 (apply orig-fn args)
        (if (memq major-mode doom-large-file-excluded-modes)
            (setq doom-large-file-p nil)
          (when (fboundp 'so-long-minor-mode) ; in case the user disabled it
            (so-long-minor-mode))
          (message "Large file! Cutting corners to improve performance")))
    (apply orig-fn args)))


(advice-add 'after-find-file :around #'doom--optimize-for-large-files-a)
(provide 'doom-large-file)
