;;; .emacs --- Will's emacs 25 init file

;;; Commentary:

;; Hacked together by someone who isn't an emacs wizard. Caveat lector.

;;; Code:

;; -- Packages -----------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar local-packages '(ag dash vc-darcs exec-path-from-shell f flycheck git
  git-gutter git-blame haskell-mode inflections jump magit markdown-mode org
  paredit pkg-info s slime undo-tree whitespace-cleanup-mode yasnippet))

(dolist (p local-packages)
  (or (package-installed-p p)
      (when (y-or-n-p (format "Package %s is missing. Install it? " p))
	(package-install p))))

;; -- Built-in Options ---------------------------------------------------------

(require 'uniquify)

;; -- Lisp Mode ----------------------------------------------------------------
(add-hook 'lisp-mode-hook
  (lambda ()
    (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'lisp-mode-hook (lambda () (show-paren-mode 1)))

;; -- Slime --------------------------------------------------------------------
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-clipboard))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;; Stop SLIME's REPL from grabbing DEL,
          ;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; -- Org Journal --------------------------------------------------------------

(require 'org-journal)
(setq org-journal-dir "~/Documents/journal/")
(setq org-journal-date-format "%A, %Y-%m-%d")


;; -- Darcs --------------------------------------------------------------------

(require 'vc-darcs)

;; -- Eshell -------------------------------------------------------------------

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
;; Emacs 25 made this change for shell, but not eshell
(setq display-buffer-alist '(("\\`\\*eshell" display-buffer-pop-up-window)))

;; -- IDO ----------------------------------------------------------------------
(require 'ido)

(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-ignore-files '("__pycache__"))
(setq ido-ignore-extensions t)
(setq completion-ignored-extensions '("pdf" "doc" "png" "pyc" "o"))
(ido-mode 1)

;; -- Visual settings ----------------------------------------------------------
(setq inhibit-splash-screen t)
(setq initial-frame-alist '((width . 157) (height . 47)))
(add-to-list 'default-frame-alist '(height . 47))
(add-to-list 'default-frame-alist '(width . 157))
(load-theme 'will t)
(blink-cursor-mode 0)
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)
(setq cursor-type 'box)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(setq ns-use-srgb-colorspace t)

;; N.B. To get antialiasing to be not insane on OS X, the following works:
;; defaults write org.gnu.Emacs AppleAntiAliasingThreshold 2
;; There doesn't seem to be a big difference in values between 1 and 4, but much
;; larger numbers turn off antialising for most faces.

;; -- Linum Mode ---------------------------------------------------------------
(global-linum-mode 1)
(setq linum-format "%d ")
(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))

;; -- Annoying Things ----------------------------------------------------------
(setq ring-bell-function 'ignore)
(setq blink-matching-delay 0.25)
(setq auto-save-default nil)
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)

;; -- Undo and Editing ---------------------------------------------------------
(global-set-key (kbd "C-c u") 'undo-tree-visualize)
(undo-tree-mode t)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(setq-default fill-column 80)
(setq tab-width 4)
(electric-pair-mode 1)
(delete-selection-mode 1)

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word. With prefix ARG
  delete across newlines as well.  The only danger in this is that you
  don't have to actually be at the end of a word to make it work.  It
  skips over to the next whitespace and then whacks it all to the next
  word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
  (re-search-forward regexp nil t)
  (replace-match "" nil nil)))
(global-set-key (kbd "C-c w") 'whack-whitespace)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
  there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
  (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region-or-line)
;; Or for the GUI Emacs ...
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

;; -- Markdown -----------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; -- HTML ---------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.tpl$" . html-mode))

;; -- C ------------------------------------------------------------------------
(setq c-default-style "k&r" c-basic-offset 4)

;; -- Structured Haskell Mode --------------------------------------------------
;; (add-to-list 'load-path "/Users/will/src/structured-haskell-mode/elisp")
;; (require 'shm)

;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; -- Scrolling ----------------------------------------------------------------
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Auto-reload changed files
(global-auto-revert-mode t)

;; Keep C-x k from prompting unless there have been changes
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)

;; Search with ag
(global-set-key (kbd "C-c s") 'ag)

;; Magit -----------------------------------------------------------------------
(global-set-key (kbd "C-c g") 'magit-status)

;; -- Org Mode -----------------------------------------------------------------
(setq org-hide-leading-stars t)

;; -- GUI Settings -------------------------------------------------------------
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(if (memq window-system '(mac ns))
  (menu-bar-mode 1)
  (menu-bar-mode 0))

;; Custom stuff ----------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (slime yasnippet whitespace-cleanup-mode vc-darcs undo-tree paredit org-journal markdown-mode magit jump haskell-mode git-gutter git-blame git geiser flycheck flx-ido exec-path-from-shell darcsum ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-add ((t (:inherit diff-added :foreground "black"))))
 '(magit-diff-del ((t (:inherit diff-removed :foreground "black"))))
 '(magit-diff-file-header ((t (:inherit diff-file-header :foreground "black"))))
 '(magit-diff-hunk-header ((t (:inherit diff-hunk-header :foreground "black")))))
