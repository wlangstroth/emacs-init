;;; will-theme.el --- Custom face theme for Emacs  -*-coding: utf-8 -*-

;;; Commentary:
;; Copyright (C) 2011-2013 Free Software Foundation, Inc.
;; Author: Kristoffer Grönlund <krig@koru.se>,
;; modified by Will Langstroth

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(deftheme will
  "Crazy-pants colours with a black background. Adapted from wombat. Basic,
  Font Lock, Isearch, Message, and Ansi-Color faces are included.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'will
   `(default ((,class (:background "#000000" :foreground "beige"))))
   `(cursor ((,class (:background "#656565"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#303030"))))
   `(highlight ((,class (:background "#454545" :foreground "#ffffff"
			 :underline t))))
   `(region ((,class (:background "#444" :foreground "#f6f3e8"))))
   `(secondary-selection ((,class (:background "#333366" :foreground "#f6f3e8"))))
   `(isearch ((,class (:background "#444" :foreground "#857b6f"))))
   `(lazy-highlight ((,class (:background "#384048" :foreground "#a0a8b0"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#444" :foreground "#f6f3e8"))))
   `(mode-line-inactive ((,class (:background "#444" :foreground "#857b6f"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#e5786d"))))
   `(escape-glyph ((,class (:foreground "#ddaa6f" :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "dodger blue"))))
   `(font-lock-keyword-face ((,class (:foreground "dark orange"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "gray41"))))
   `(font-lock-comment-face ((,class (:foreground "gray45" :italic t))))
   `(font-lock-constant-face ((,class (:foreground "dodger blue"))))
   `(font-lock-doc-face ((,class (:foreground "gray45"))))
   `(font-lock-function-name-face ((,class (:foreground "turquoise"))))
   `(font-lock-negation-char-face ((,class (:foreground "blue violet"))))
   `(font-lock-preprocessor-face ((,class (:foreground "#5555ff"))))
   `(font-lock-regexp-grouping-backslash ((,class (:bold t :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:bold t :weight bold))))
   `(font-lock-string-face ((,class (:foreground "green"))))
   `(font-lock-type-face ((,class (:foreground "deep sky blue"))))
   `(font-lock-variable-name-face ((,class (:foreground "#0553a1"))))
   `(font-lock-warning-face ((,class (:bold t :foreground "pink" :weight bold))))
   ;; Button and link faces
   `(link ((,class (:foreground "#8ac6f2" :underline t))))
   `(link-visited ((,class (:foreground "#e5786d" :underline t))))
   `(button ((,class (:background "#333333" :foreground "#f6f3e8"))))
   `(header-line ((,class (:background "#303030" :foreground "#e7f6da"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground "#8ac6f2" :weight bold))))
   `(message-header-cc ((,class (:foreground "#95e454"))))
   `(message-header-other ((,class (:foreground "#95e454"))))
   `(message-header-subject ((,class (:foreground "#cae682"))))
   `(message-header-to ((,class (:foreground "#cae682"))))
   `(message-cited-text ((,class (:foreground "#99968b"))))
   `(message-separator ((,class (:foreground "#e5786d" :weight bold))))
   ;; Org faces
   `(outline-1 ((,class (:foreground "#55ff55"))))
   `(outline-2 ((,class (:foreground "#008ed1"))))
   `(outline-3 ((,class (nil))))
   `(outline-4 ((,class (:foreground "#f92672"))))
   `(outline-5 ((,class (:foreground "#b9fc6d"))))
   `(outline-6 ((,class (:foreground "purple"))))
   `(outline-7 ((,class (:foreground "#5555ff"))))
   `(outline-8 ((,class (:foreground "#ffff55"))))))

(custom-theme-set-variables
 'will
 '(ansi-color-names-vector ["#242424" "#0553a1" "#4f863f" "#c17248"
			    "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'will)
;;; will-theme.el ends here
