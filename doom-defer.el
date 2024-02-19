;; This originates from doom emacs, but I found it in Noctuid's config.
;; https://github.com/hlissner/doom-emacs/blob/42a21dffddeee57d84e82a9f0b65d1b0cba2b2af/core/core.el#L353
(require 'use-package)

(defvar doom-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticeable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be
broken up into:
  (doom-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))
This is already done by the lang/org module, however.
If you want to disable incremental loading altogether, either remove
`doom-load-packages-incrementally-h' from `emacs-startup-hook' or set
`doom-incremental-first-idle-timer' to nil.")

(defvar doom--deferred-packages-alist '(t))

(defvar doom-incremental-first-idle-timer 2.0
  "How long (in idle seconds) until incremental loading starts.
Set this to nil to disable incremental loading.")

(defvar doom-incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defvar doom-incremental-load-immediately nil
  ;; (daemonp)
  "If non-nil, load all incrementally deferred packages immediately at startup.")
  
(defvar doom-inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `doom-log' output.")

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))
  
(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher
                          `(funcall ,fetcher ,elt ,list)
                        elt)
                     ,list)))
		     
(defvar doom-context '(t)
  "A list of symbols identifying all active Doom execution contexts.

This should never be directly changed, only let-bound, and should never be
empty. Each context describes what phase Doom is in, and may respond to.

All valid contexts:
  cli        -- while executing a Doom CLI
  compile    -- while byte-compilation is in progress
  eval       -- during inline evaluation of elisp
  init       -- while doom is formally starting up for the first time, after its
                core libraries are loaded, but before user config is.
  modules    -- while loading modules and their files
  sandbox    -- This session was launched from Doom's sandbox.
  packages   -- when packagedefs are being read
  reload     -- while reloading doom")
(put 'doom-context 'valid-values '(cli compile eval init modules packages reload doctor sandbox))
(put 'doom-context 'risky-local-variable t)

(defun doom-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.
If NOW is non-nil, load PACKAGES incrementally, in `doom-incremental-idle-timer'
intervals."
  (if (not now)
      (appendq! doom-incremental-packages packages)
    (while packages
      (let ((req (pop packages)))
        (unless (featurep req)
          (message "Incrementally loading %s" req)
          (condition-case e
              (or (while-no-input
                    ;; If `default-directory' is a directory that doesn't exist
                    ;; or is unreadable, Emacs throws up file-missing errors, so
                    ;; we set it to a directory we know exists and is readable.
                    (let ((default-directory user-emacs-directory)
                          (gc-cons-threshold most-positive-fixnum)
                          file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            ((error debug)
             (message "Failed to load '%s' package incrementally, because: %s"
                      req e)))
          (if (not packages)
              (message "Finished incremental loading")
            (run-with-idle-timer doom-incremental-idle-timer
                                 nil #'doom-load-packages-incrementally
                                 packages t)
            (setq packages nil)))))))

(defun doom-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `doom-incremental-packages'.
If this is a daemon session, load them all immediately instead."
  (if doom-incremental-load-immediately
      (mapc #'require (cdr doom-incremental-packages))
    (when (numberp doom-incremental-first-idle-timer)
      (run-with-idle-timer doom-incremental-first-idle-timer
                           nil #'doom-load-packages-incrementally
                           (cdr doom-incremental-packages) t))))

(add-hook 'emacs-startup-hook #'doom-load-packages-incrementally-h)

(defmacro doom-log (message &rest args)
  "Log a message in *Messages*.

Does not emit the message in the echo area. This is a macro instead of a
function to prevent the potentially expensive evaluation of its arguments when
debug mode is off. Return non-nil."
  (declare (debug t))
  `(unless doom-inhibit-log (doom--log ,message ,@args)))

(defun doom--log (text &rest args)
  (let ((inhibit-message (not init-file-debug))
        (absolute? (string-prefix-p ":" text)))
    (apply #'message
           (propertize (concat "* %.06f:%s" (if (not absolute?) ":") text)
                       'face 'font-lock-doc-face)
           (float-time (time-subtract (current-time) before-init-time))
           (mapconcat
            (lambda (x) (format "%s" x))
            (unless absolute?
              (append (cons '* (remq t (reverse doom-context)))
                      (if (bound-and-true-p doom-module-context)
                          (let ((key (doom-module-context-key)))
                            (delq nil (list (car key) (cdr key)))))))
            ":")
           args)))

;; Adds two keywords to `use-package' to expand its lazy-loading capabilities:
;;
;;   :after-call SYMBOL|LIST
;;   :defer-incrementally SYMBOL|LIST|t
;;
;; Check out `use-package!'s documentation for more about these two.
(with-eval-after-load 'use-package
  (eval-when-compile
    (dolist (keyword '(:defer-incrementally :after-call))
      (push keyword use-package-deferring-keywords)
      (setq use-package-keywords
            (use-package-list-insert keyword use-package-keywords :after)))

    (defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
    (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
      (use-package-concat
       `((doom-load-packages-incrementally
          ',(if (equal targets '(t))
		(list name)
              (append targets (list name)))))
       (use-package-process-keywords name rest state)))
    
    (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)
    (defun use-package-handler/:after-call (name _keyword hooks rest state)
      (if (plist-get state :demand)
          (use-package-process-keywords name rest state)
	(let ((fn (make-symbol (format "doom--after-call-%s-h" name))))
          (use-package-concat
           `((fset ',fn
                   (lambda (&rest _)
                     (doom-log "use-package: lazy loading %s from %s" ',name ',fn)
                     (condition-case e
			 ;; If `default-directory' is a directory that doesn't
			 ;; exist or is unreadable, Emacs throws up file-missing
			 ;; errors, so we set it to a directory we know exists and
			 ;; is readable.
			 (let ((default-directory user-emacs-directory))
                           (require ',name))
                       ((debug error)
			(message "Failed to load deferred package %s: %s" ',name e)))
                     (when-let (deferral-list (assq ',name doom--deferred-packages-alist))
                       (dolist (hook (cdr deferral-list))
			 (advice-remove hook #',fn)
			 (remove-hook hook #',fn))
                       (delq! deferral-list doom--deferred-packages-alist)
                       (unintern ',fn nil)))))
           (let (forms)
             (dolist (hook hooks forms)
               (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
			 `(add-hook ',hook #',fn)
                       `(advice-add #',hook :before #',fn))
                     forms)))
           `((unless (assq ',name doom--deferred-packages-alist)
               (push '(,name) doom--deferred-packages-alist))
             (nconc (assq ',name doom--deferred-packages-alist)
                    '(,@hooks)))
           (use-package-process-keywords name rest state)))))))

(provide 'doom-defer)
