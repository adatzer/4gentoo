;; Ada


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
;; BASIC -----------------------------------------------------------------------
;; -----

;; Don't make backups
(setq make-backup-files 0 backup-inhibited t)


;; menu-bar-mode, tooltip-mode, tool-bar-mode, column-number-mode
(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(setq column-number-mode t)


;; display line-numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))


;; which-key
(straight-use-package 'which-key)
(which-key-mode)


;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)



;; -----
;; SLIME -----------------------------------------------------------------------
;; -----
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq slime-contribs '(slime-fancy))
(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl"))
	(ccl ("/usr/bin/ccl"))
	(ecl ("/usr/bin/ecl"))))
;(setq inferior-lisp-program "sbcl")

;; SLDB
;; display slime debugger for sbcl (level 1) below selected
(push '("\\*sldb sbcl/1\\*"
	(display-buffer-below-selected))
      display-buffer-alist)



;; -----
;; OTHER -----------------------------------------------------------------------
;; -----

;; htmlize
(straight-use-package 'htmlize)

;; rainbow-mode
(straight-use-package 'rainbow-mode)

;; emacs-reveal
(straight-use-package 'org-re-reveal)

;; ess
(straight-use-package 'ess)

;; org-mode and necessary keys
(straight-use-package 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;; babel
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (scheme . t)
   (R . t)
   (python . t)
   (sql . t)
   (shell . t)))



;; --------------------------------------------
;; KEYS - ALIASES ..following slime conventions --------------------------------
;; --------------------------------------------

;; M-x sclear RET
(defalias 'sclear 'slime-repl-clear-buffer)

;; C-x C-a C-c == C-x C-a c
(global-set-key [(control ?x) (control ?a) (control ?c)] 'slime-repl-clear-buffer)
(global-set-key [(control ?x) (control ?a) ?c]           'slime-repl-clear-buffer)

;; C-x C-a C-d == C-x C-a d
(global-set-key [(control ?x) (control ?a) (control ?d)] 'slime-edit-definition)
(global-set-key [(control ?x) (control ?a) ?d]           'slime-edit-definition)

;; C-x C-a C-s == C-x C-a s
(global-set-key [(control ?x) (control ?a) (control ?s)] 'slime-pop-find-definition-stack)
(global-set-key [(control ?x) (control ?a) ?s]           'slime-pop-find-definition-stack)

;; comment-or-uncomment-region
(global-set-key "\C-c;" 'comment-or-uncomment-region)

;; C-j
(substitute-key-definition
 'electric-newline-and-maybe-indent
 'previous-line
 (current-global-map))          ; C-j == previous-line
(substitute-key-definition
 'slime-repl-newline-and-indent
 'previous-line
 slime-repl-mode-map)           ; also for slime
(substitute-key-definition
 'org-return-indent
 'previous-line
 org-mode-map)                  ; also for org-mode

;; C-k
(substitute-key-definition
 'kill-line 'next-line (current-global-map))  ; C-k == next-line

;; C-, vs C-. vs C-> vs C-<
(global-set-key [(control ?,)] 'backward-char) ; C-, == backward-char
(substitute-key-definition                     ; also for org-mode
 'org-cycle-agenda-files
 'backward-char
 org-mode-map)
(global-set-key [(control ?.)] 'forward-char)  ; C-. == forward-char
(global-set-key [(control ?<)] 'backward-word) ; C-< == backward-word
(global-set-key [(control ?>)] 'forward-word)  ; C-> == forward-word



;; ------
;; THEMES ---------------------------------------------------------------------- 
;; ------

;; gocyda-theme
(straight-use-package
 `(gocyda-theme :type git :host github :repo "adatzer/gocyda-theme"))
;(load-theme 'gocyda t)


;; nodra-theme
(straight-use-package
 `(nodra-theme :type git :host github :repo "adatzer/nodra-theme"))
(load-theme 'nodra t)




;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------