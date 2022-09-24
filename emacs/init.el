;;;; Ada


;; bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)



;; -----
;; BASIC ------------------------------------------------------------------
;; -----

;; Don't make backups
(setq make-backup-files 0 backup-inhibited t)

;; before-save-hook
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; real-auto-save
(straight-use-package 'real-auto-save)
(setq real-auto-save-interval 3) ;; in seconds
(add-hook 'fundamental-mode-hook 'real-auto-save-mode)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'change-log-mode-hook 'real-auto-save-mode)
(add-hook 'text-mode-hook 'real-auto-save-mode)

;; assign to prog-mode
(add-to-list 'auto-mode-alist '("\\.hcl\\'" . prog-mode))
(add-to-list 'auto-mode-alist '("\\.tf\\'" . prog-mode))
(add-to-list 'auto-mode-alist '("\\.tfvars\\'" . prog-mode))
(add-to-list 'auto-mode-alist '("\\.yml.tmpl\\'" . prog-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . prog-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . prog-mode))

;; always get the latest changes from disk
(global-auto-revert-mode t)

(setq auto-save-default nil)
(setq create-lockfiles nil)

;; autosave, without the #autosave-files#, see ergoemacs
;; (defun now-save-all-unsaved ()
;;   (interactive)
;;   (save-some-buffers t))

;; (add-hook 'focus-out-hook (lambda () (now-save-all-unsaved)))
;; (add-function :after after-focus-change-function #'now-save-all-unsaved)

;; final newline
;; (setq-default require-final-newline nil)
;; (setq mode-require-final-newline nil)

;; Startup
(setq inhibit-startup-screen  t
      inhibit-startup-message t
      initial-scratch-message "")

;; tabs
(setq-default indent-tabs-mode nil)

;; whitespace
(setq show-trailing-whitespace t)

;; scroll
(setq auto-window-vscroll   nil
      scroll-conservatively 5)

;; menu-bar-mode, tooltip-mode, tool-bar-mode, column-number-mode
(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq column-number-mode t)

;; show some path on top
(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

;; electric-pair-mode ...see also the SLIME, SLY sections below
(electric-pair-mode 1)

;; display line-numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program  "surf")


;; which-key
(straight-use-package 'which-key)
(which-key-mode)


;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)



;; ----
;; LISP -------------------------------------------------------------------
;; ----
(setq lisp-lambda-list-keyword-parameter-alignment t
      lisp-lambda-list-keyword-alignment           t)

(add-hook 'lisp-mode-hook
          (lambda ()
            (setq lisp-indent-function 'common-lisp-indent-function)))

;;(setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/HyperSpec/")



;; -----
;; SLIME ------------------------------------------------------------------
;; -----
;; (add-to-list 'load-path (expand-file-name "~/common-lisp/slime"))
;; (require 'slime-autoloads)

;; (setq slime-contribs '(slime-fancy slime-cl-indent))
;; (slime-setup)

;; (setq inferior-lisp-program "sbcl --dynamic-space-size 16000")
;; (setq inferior-lisp-program "ccl")

;;(setq slime-lisp-implementations '((ccl ("/usr/bin/ccl"))
;;				   (sbcl ("/usr/bin/sbcl")
;;                                         :coding-system utf-8-unix)
;;				   (ecl ("/usr/bin/ecl"))))
;; (setq slime-net-coding-system 'utf-8-unix)

;; (add-to-list 'slime-completion-at-point-functions
;; 	     'slime-fuzzy-complete-symbol)

;; slime-repl-return behavior depends on balanced parens, so:
;; (add-hook 'slime-repl-mode-hook
;; 	  (lambda ()
;; 	    (electric-pair-local-mode -1)))
;; SLDB
;; display slime debugger for sbcl and ccl below selected
;; (setq display-buffer-alist
;;       '(("sldb"
;;          (display-buffer-below-selected)
;;          (side . right)
;;          (slot . 1)
;;          (window-width . 0.5)
;;          (reusable-frames . nil))))


;; ---
;; SLY --------------------------------------------------------------------
;; ---
;; (straight-use-package 'sly)
;; (setq sly-net-coding-system 'utf-8-unix)
;; (add-hook 'sly-mrepl-mode-hook
;; 	  (lambda ()
;; 	    (electric-pair-local-mode -1)))



;; -------
;; CLOJURE ----------------------------------------------------------------
;; -------
(straight-use-package 'clojure-mode)

(straight-use-package 'cider)



;; ------
;; RACKET -----------------------------------------------------------------
;; ------
(straight-use-package 'racket-mode)



;; ----
;; HTML -------------------------------------------------------------------
;; ----
(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)))



;; ----------------
;; JAVASCRIPT - JSX -------------------------------------------------------
;; ----------------
(straight-use-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js-indent-level 2)

(straight-use-package 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

(straight-use-package 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(setq typescript-indent-level 2)



;; ----
;; JSON -------------------------------------------------------------------
;; ----
(straight-use-package 'json-mode)
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 8)))

(defun json-indent-4()
  (interactive)
  (setq js-indent-level 4))

(defun json-indent-8()
  (interactive)
  (setq js-indent-level 8))



;; ----
;; GO ---------------------------------------------------------------------
;; ----
(straight-use-package 'go-mode)

;; example
;; (add-hook 'go-mode-hook
;;   (lambda ()
;;     (setq-default)
;;     (setq tab-width 2)
;;     (setq standard-indent 2)
;;     (setq indent-tabs-mode nil)))



;; ----
;; HCL --------------------------------------------------------------------
;; ----
(straight-use-package 'hcl-mode)
(custom-set-variables
 '(hcl-indent-level 2))


;; ----
;; LUA --------------------------------------------------------------------
;; ----
(straight-use-package 'lua-mode)



;; ----
;; SCALA ------------------------------------------------------------------
;; ----
(straight-use-package 'scala-mode)



;; ----
;; PHP --------------------------------------------------------------------
;; ----
(straight-use-package 'php-mode)



;; ----
;; RUST -------------------------------------------------------------------
;; ----
(straight-use-package 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))



;; ----
;; JAVA -------------------------------------------------------------------
;; ----
(defun java-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'java-indent-setup)



;; ----
;; OBJC -------------------------------------------------------------------
;; ----
(defun objc-indent-setup ()
  (c-set-style "java")
  (c-set-offset 'brace-list-close '-)
  (c-set-offset 'brace-list-intro '0)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+))
(add-hook 'objc-mode-hook 'objc-indent-setup)



;; ------------------
;; rainbow-delimiters -----------------------------------------------------
;; ------------------
(straight-use-package 'rainbow-delimiters)

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)



;; -------
;; NEOTREE ----------------------------------------------------------------
;; -------
(straight-use-package
 `(neotree :type git :host github :repo "jaypei/emacs-neotree"))
;; also see keybindings below
(setq neo-theme (if (display-graphic-p) 'ascii))

(setq neo-window-width          40
      neo-smart-open            nil
      neo-toggle-window-keep-p  t
      neo-autorefresh           nil
      neo-show-hidden-files     t
      neo-auto-indent-point     t)

(add-hook 'neotree-mode-hook
 	  (lambda (&rest args)
 	    (display-line-numbers-mode -1)))

(add-hook 'neotree-mode-hook
	  (lambda ()
	    (hl-line-mode 1)))



;; -----
;; OTHER ------------------------------------------------------------------
;; -----

;; markdown-mode
(straight-use-package 'markdown-mode)

;; htmlize
(straight-use-package 'htmlize)

;; rainbow-mode
(straight-use-package 'rainbow-mode)

;; ess
;; (straight-use-package 'ess)

;; org-mode and necessary keys
;(straight-use-package 'org)
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-ca" 'org-agenda)
;(global-set-key "\C-cc" 'org-capture)
;(global-set-key "\C-cb" 'org-switchb)

;; babel
;; active Babel languages
;(org-babel-do-load-languages
; 'org-babel-load-languages
; '((lisp . t)
;   (scheme . t)
;   (R . t)
;   (python . t)
;   (sql . t)
;   (shell . t)))

;; emacs-reveal
;(straight-use-package 'org-re-reveal)



;; ---------
;; FUN STAFF --------------------------------------------------------------
;; ---------

;; minibuffer background color
(defun minibuffer-bg ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :background "#000000" :foreground "#ffffff"))))
(add-hook 'minibuffer-setup-hook 'minibuffer-bg)

;; swap windows (from github: vseloved/scripts)
(defun swap-windows ()
  "If you have 2 windows, it swaps them"
  (interactive)
  (cond ((/= (count-windows) 2)
	 (message "You need exactly 2 windows to do this."))
	(t
	 (let* ((w1 (first (window-list)))
		(w2 (second (window-list)))
		(b1 (window-buffer w1))
		(b2 (window-buffer w2))
		(s1 (window-start w1))
		(s2 (window-start w2)))
	   (set-window-buffer w1 b2)
	   (set-window-buffer w2 b1)
	   (set-window-start w1 s2)
	   (set-window-start w2 s1))))
  (other-window 1))

(defun enclose-forward-sexp-in-parens ()
  "Encloses following sexp in parens.Places the cursor after the opening one"
  (interactive)
  (insert "(")
  (forward-sexp 1)
  (insert ")")
  (backward-sexp 1)
  (forward-char 1))

(defun enclose-forward-sexp-in-dquotes ()
  "Encloses following sexp in dquotes.Places the cursor after the opening one"
  (interactive)
  (insert "\"")
  (forward-sexp 1)
  (insert "\"")
  (backward-sexp 1)
  (forward-char 1))

(defun enclose-forward-sexp-in-squotes ()
  "Encloses following sexp in squotes.Places the cursor after the opening one"
  (interactive)
  (insert "'")
  (forward-sexp 1)
  (insert "'")
  (backward-sexp 1)
  (forward-char 1))

(defun enclose-forward-sexp-in-braces ()
  "Encloses following sexp in braces.Places the cursor after the opening one"
  (interactive)
  (insert "[")
  (forward-sexp 1)
  (insert "]")
  (backward-sexp 1)
  (forward-char 1))



;; --------------
;; KEYS - ALIASES (following slime's conventions) -------------------------
;; --------------

;; M-x sclear RET for SLIME
;; (defalias 'sclear 'slime-repl-clear-buffer)

;; M-x xclear RET for SLY
;; (defalias 'xclear 'sly-mrepl-clear-repl)

;; C-c s  :: swap-windows
(global-set-key [(control ?c) ?s] 'swap-windows)

;; C-x C-a C-p  :: enclose-forward-sexp-in-parens
(global-set-key [(control ?x) (control ?a) ?p]
		'enclose-forward-sexp-in-parens)
(global-set-key [(control ?x) (control ?a) (control ?p)]
		'enclose-forward-sexp-in-parens)

;; C-x C-a C-o  :: enclose-forward-sexp-in-dquotes
(global-set-key [(control ?x) (control ?a) ?o]
		'enclose-forward-sexp-in-dquotes)
(global-set-key [(control ?x) (control ?a) (control ?o)]
		'enclose-forward-sexp-in-dquotes)

;; C-x C-a C-o  :: enclose-forward-sexp-in-squotes
(global-set-key [(control ?x) (control ?a) ?i]
		'enclose-forward-sexp-in-squotes)
(global-set-key [(control ?x) (control ?a) (control ?i)]
		'enclose-forward-sexp-in-squotes)

;; C-x C-a C-u  :: enclose-forward-sexp-in-braces
(global-set-key [(control ?x) (control ?a) ?u]
		'enclose-forward-sexp-in-braces)
(global-set-key [(control ?x) (control ?a) (control ?u)]
		'enclose-forward-sexp-in-braces)

;; <f7>  :: neotree-toggle
(global-set-key [f7] 'neotree-toggle)

;; C-c ;  :: comment-or-uncomment-region
(global-set-key "\C-c;" 'comment-or-uncomment-region)

;; C-`  :: other-window
(global-set-key [(control ?`)] 'other-window)

;; C-x C-a C-c == C-x C-a c  :: slime-repl-clear-buffer
;; (global-set-key [(control ?x) (control ?a) (control ?c)]
;;                 'slime-repl-clear-buffer)
;; (global-set-key [(control ?x) (control ?a) ?c]
;;                 'slime-repl-clear-buffer)

;; C-x C-a C-d == C-x C-a d  :: slime-edit-definition
;; (global-set-key [(control ?x) (control ?a) (control ?d)] 'slime-edit-definition)
;; (global-set-key [(control ?x) (control ?a) ?d]           'slime-edit-definition)

;; C-x C-a C-s == C-x C-a s  :: slime-pop-find-definition-stack
(global-set-key [(control ?x) (control ?a) (control ?s)]
                'slime-pop-find-definition-stack)
(global-set-key [(control ?x) (control ?a) ?s]
                'slime-pop-find-definition-stack)

;; C-x C-a C-e == C-x C-a e  :: end-of-buffer
(global-set-key [(control ?x) (control ?a) (control ?e)] 'end-of-buffer)
(global-set-key [(control ?x) (control ?a) ?e]           'end-of-buffer)

;; C-j  :: previous-line
(substitute-key-definition
 'electric-newline-and-maybe-indent
 'previous-line
 (current-global-map)) ; C-j == previous-line
;; (substitute-key-definition
;;  'slime-repl-newline-and-indent
;;  'previous-line
;;  slime-repl-mode-map)  ; also for slime
;(substitute-key-definition
; 'org-return-indent
; 'previous-line
; org-mode-map)         ; also for org-mode

;; C-k  :: next-line
(substitute-key-definition
 'kill-line 'next-line (current-global-map))  ; C-k == next-line

;; C-, vs C-. vs C-> vs C-<
(global-set-key [(control ?,)] 'backward-char) ; C-, == backward-char
;(substitute-key-definition                     ; also for org-mode
 ;'org-cycle-agenda-files
 ;'backward-char
 ;org-mode-map)
(global-set-key [(control ?.)] 'forward-char)  ; C-. == forward-char
(global-set-key [(control ?<)] 'backward-word) ; C-< == backward-word
(global-set-key [(control ?>)] 'forward-word)  ; C-> == forward-word

;; C-p  :: forward-sexp
(global-unset-key [(control ?p)])
(global-set-key [(control ?p)] 'forward-sexp)

;; C-o  :: backward-sexp
(global-unset-key [(control ?o)])
(global-set-key [(control ?o)] 'backward-sexp)



;; ------
;; THEMES -----------------------------------------------------------------
;; ------

;; nodra-theme
(add-to-list 'custom-theme-load-path (expand-file-name "path/to/nodra"))
(load-theme 'nodra t)



;; -------
;; STARTUP ----------------------------------------------------------------
;; -------

(neotree)



;; ------------------------------------------------------------------------
;; ------------------------------------------------------------------------
;; ------------------------------------------------------------------------

(set-face-attribute 'default nil :height 110)
;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
