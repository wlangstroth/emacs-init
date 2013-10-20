;;; .emacs --- Will's emacs 24 init file

;;; Commentary:

; Hacked together by someone who isn't an emacs wizard. Caveat lector.

;;; Code:

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(require 'cl) ; For fiplr and grizzl

;; -- Visual settings ----------------------------------------------------------
(setq inhibit-splash-screen t)
(setq initial-frame-alist '((width . 202) (height . 60)))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 202))
(load-theme 'will t)
(global-linum-mode 1)
(setq linum-format "%4d ")
(setq column-number-mode t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(blink-cursor-mode 0)
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; -- Annoying Things ----------------------------------------------------------
(setq ring-bell-function 'ignore)
(setq auto-save-default nil)
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -- Steve Yegge Section ------------------------------------------------------
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(global-set-key (kbd "C-c f") 'fiplr-find-file)
(setq fiplr-root-markers '(".git" ".hg"))
(setq fiplr-ignored-globs '((directories (".git" ".hg" "tmp" "log"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

;; -- Undo and Editing ---------------------------------------------------------
(global-set-key (kbd "C-c u") 'undo-tree-visualize)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(set-fill-column 80)
(setq tab-width 4)
(electric-pair-mode 1)
(define-key global-map (kbd "RET") 'newline-and-indent)

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

;; (defun beginning-of-line-or-indentation ()
;;   "Move to beginning of line, or indentation"
;;   (interactive)
;;   (if (bolp)
;;       (back-to-indentation)
;;     (beginning-of-line)))
;; (global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

;; -- Ruby File Types ----------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Auto-reload changed files
(global-auto-revert-mode t)

;; Keep C-x k from prompting unless there have been changes
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c p") 'previous-buffer)

;; ISO, dammit
;; (calendar-set-date-style 'iso)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; -- Org Mode -----------------------------------------------------------------
(setq org-hide-leading-stars t)

;; -- Packages -----------------------------------------------------------------
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(ag dash exec-path-from-shell f findr fiplr flycheck grizzl haml-mode
      haskell-mode rainbow-mode hi2 inf-ruby inflections jump markdown-mode org
      paredit pkg-info pkg-info rbenv ruby-compilation ruby-electric ruby-end s
      slim-mode undo-tree w3m whitespace-cleanup-mode yaml-mode yasnippet))

;; -- Font for GUI -------------------------------------------------------------
(set-face-attribute 'default nil :font "Menlo-12")
