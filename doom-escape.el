(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape (&optional interactive)
  "Universal, non-nuclear escape

`keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
do-what-I-mean. It serves four purposes (in order):

1. Quit active states; e.g. highlights, searches, snippets, iedit,
   multiple-cursors, recording macros, etc.
2. Close popup windows remotely (if it is allowed to)
3. Refresh buffer indicators, like git-gutter and flycheck
4. Or fall back to `keyboard-quit'

And it should do these things incrementally, rather than all at once. And it
shouldn't interfere with recording macros or the minibuffer. This may require
you press ESC/C-g two or three times on some occasions to reach
`keyboard-quit', but this is much more intuitive."
  
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'doom-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

(global-set-key [remap keyboard-quit] #'doom/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape))

(add-hook 'doom-escape-hook 'transient-quit-one)

(provide 'doom-escape)
