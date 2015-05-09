;;; .emacs --- Will's emacs 24 init file

;;; Commentary:

;; Hacked together by someone who isn't an emacs wizard. Caveat lector.

;;; Code:

;; -- Packages -----------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; To refresh, go to the end of the expression and C-x C-e.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(ag company-ghc dash exec-path-from-shell f findr flx flx-ido flycheck git
      haskell-mode inflections jump magit markdown-mode org paredit pkg-info s
      undo-tree whitespace-cleanup-mode yasnippet))

(require 'ido)
(ido-mode t)

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
;; defaults write org.gnu.Emacs AppleAntiAliasingThreshold 2 There doesn't seem
;; to be a big difference in values between 1 and 4, but much larger numbers
;; turn off antialising for all intents and purposes.

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
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote
       ("\n-> " "" "\n   " "\n   ..." "[" "]"
	" [No match]" " [Matched]" " [Not readable]"
	" [Too big]" " [Confirm]")))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

(defun ido-define-keys () ; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(add-hook 'ido-setup-hook 'ido-define-keys)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(global-company-mode t)

;; -- Markdown -----------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

;; -- Haskell Mode -------------------------------------------------------------
(defun haskell-style ()
  "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4))

(add-hook 'haskell-mode-hook 'haskell-style)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;; Don't show the underscores
(add-hook 'haskell-mode-hook 'haskell-indentation-disable-show-indentations)

;; -- Lisp Mode ----------------------------------------------------------------
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (local-set-key (kbd "RET") 'newline-and-indent)))

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

;; -- Org Mode -----------------------------------------------------------------
(setq org-hide-leading-stars t)

;; -- Geiser Settings ----------------------------------------------------------
(setq geiser-active-implementations '(racket))
(setq geiser-mode-autodoc-p nil)

;; -- GUI Settings -------------------------------------------------------------
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(if (memq window-system '(mac ns))
  (menu-bar-mode 1)
  (menu-bar-mode 0))
(when (memq window-system '(mac ns))
  (set-face-attribute 'default nil :font "Inconsolata-Medium-15"))
