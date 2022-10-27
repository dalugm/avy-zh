;;; avy-zh.el --- Jump to Chinese characters using avy

;; Author: dalu <mou.tong@qq.com>
;; Maintainer: dalu <mou.tong@qq.com>
;; Version: 0.1.0
;; Package-Requires: ((avy "0.4.0") (zh-lib "0.1.0"))
;; Homepage: https://github.com/dalugm/evil-zh
;; Keywords: Chinese, point, location

;; This file is NOT part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Jump to Chinese characters using `avy'.
;;
;; 使用 `avy' 跳转到中文字符。
;;

;;; Code:

(require 'avy)
(require 'zh-lib)

(defgroup avy-zh nil
  "Jump to Chinese characters using `avy'."
  :group 'avy)

;; TODO: make `avy-zh' support `avy-goto-char-timer'
(defcustom avy-zh-jump-word-timeout 0.5
  "Seconds to wait for input."
  :type 'float
  :group 'avy-zh)

(defcustom avy-zh-treat-word-as-char t
  "Whether word related `avy-*' commands should be remapped."
  :type 'boolean
  :group 'avy-zh)

(defvar avy-zh--original-avy-goto-char (symbol-function 'avy-goto-char)
  "Original definition of `avy-goto-char'.")

(defvar avy-zh--original-avy-goto-char-2 (symbol-function 'avy-goto-char-2)
  "Original definition of `avy-goto-char-2'.")

(defvar avy-zh--original-avy-goto-char-in-line (symbol-function 'avy-goto-char-in-line)
  "Original definition of `avy-goto-char-in-line'.")

(defvar avy-zh--original-avy-goto-word-0 (symbol-function 'avy-goto-word-0)
  "Original definition of `avy-goto-word-0'.")

(defvar avy-zh--original-avy-goto-word-1 (symbol-function 'avy-goto-word-1)
  "Original definition of `avy-goto-word-1'.")

(defvar avy-zh--original-avy-goto-subword-0 (symbol-function 'avy-goto-subword-0)
  "Original definition of `avy-goto-subword-0'.")

(defvar avy-zh--original-avy-goto-subword-1 (symbol-function 'avy-goto-subword-1)
  "Original definition of `avy-goto-subword-1'.")

(defvar avy-zh--original-avy-zh-goto-word-or-subword-1 (symbol-function 'avy-zh-goto-word-or-subword-1)
  "Original definition of `avy-zh-goto-word-or-subword-1'.")

(defun avy-zh-goto-char (char &optional arg)
  "`avy-zh' replacement of `avy-goto-char'.

Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-char
    (avy-jump (if (= 13 char)
                  "\n"
                (zh-lib-build-regexp-char
                 char
                 (not zh-lib-with-punctuation)))
              :window-flip arg)))

(defun avy-zh-goto-char-in-line (char)
  "`avy-zh' replacement of `avy-goto-char-in-line'.

Jump to the currently visible CHAR in the current line."
  (interactive (list (read-char "char: " t)))
  (avy-with avy-goto-char
    (avy-jump (zh-lib-build-regexp-char
               char
               (not zh-lib-with-punctuation))
              :beg (line-beginning-position)
              :end (line-end-position))))

(defun avy-zh-goto-char-2 (char1 char2 &optional arg beg end)
  "`avy-zh' replacement of `avy-goto-char-2'.

Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive (list (let ((c1 (read-char "char 1: " t)))
                       (if (memq c1 '(? ?\b))
                           (keyboard-quit)
                         c1))
                     (let ((c2 (read-char "char 2: " t)))
                       (cond ((eq c2 ?)
                              (keyboard-quit))
                             ((memq c2 avy-del-last-char-by)
                              (keyboard-escape-quit)
                              (call-interactively 'avy-goto-char-2))
                             (t
                              c2)))
                     current-prefix-arg
                     nil nil))
  (when (eq char1 ?)
    (setq char1 ?\n))
  (when (eq char2 ?)
    (setq char2 ?\n))
  (avy-with avy-goto-char-2
    (avy-jump (zh-lib-build-regexp-string
               (string char1 char2)
               (not zh-lib-with-punctuation))
              :window-flip arg
              :beg beg
              :end end)))

(defun avy-zh-goto-word-0 (arg)
  "`avy-zh' replacement of `avy-goto-word-0'.

Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive "P")
  (let ((avy-goto-word-0-regexp "\\b\\sw\\|\\cc"))
    (funcall avy-zh--original-avy-goto-word-0 arg)))

(defun avy-zh-goto-word-1 (char &optional arg beg end symbol)
  "`avy-zh' replacement of `avy-goto-word-1'.

Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-1
    (let* ((str (string char))
           (regex (cond
                   ((string= str ".")
                    "\\.")
                   ((and avy-word-punc-regexp
                         (string-match avy-word-punc-regexp str))
                    (regexp-quote str))
                   ((<= char 26)
                    str)
                   (t
                    (concat
                     (if symbol "\\_<" "\\b")
                     str
                     (let ((chinese-regexp (zh-lib-build-regexp-char
                                            char
                                            (not zh-lib-with-punctuation))))
                       (unless (string= chinese-regexp "")
                         (concat "\\|" chinese-regexp))))))))
      (avy-jump regex
                :window-flip arg
                :beg beg
                :end end))))

(declare-function subword-backward "subword")

(defun avy-zh-goto-subword-0 (&optional arg predicate beg end)
  "`avy-zh' replacement of `avy-goto-subword-0'.

Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it’s a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched."
  (interactive "P")
  (require 'subword)
  (avy-with avy-goto-subword-0
    (let ((case-fold-search nil)
          (subword-backward-regexp
           "\\(\\(\\W\\|[[:lower:][:digit:]]\\)\\([!-/:@`~[:upper:]]+\\W*\\)\\|\\W\\w+\\|.\\cc\\)")
          candidates)
      (avy-dowindows arg
        (let ((syn-tbl (copy-syntax-table)))
          (dolist (char avy-subword-extra-word-chars)
            (modify-syntax-entry char "w" syn-tbl))
          (with-syntax-table syn-tbl
            (let ((ws (or beg (window-start)))
                  window-cands)
              (save-excursion
                (goto-char (or end (window-end (selected-window) t)))
                (subword-backward)
                (while (> (point) ws)
                  (when (or (null predicate)
                            (and predicate (funcall predicate)))
                    (unless (not (avy--visible-p (point)))
                      (push (cons (cons (point) (1+ (point)))
                                  (selected-window)) window-cands)))
                  (subword-backward))
                (and (= (point) ws)
                     (or (null predicate)
                         (and predicate (funcall predicate)))
                     (not (get-char-property (point) 'invisible))
                     (push (cons (cons (point) (1+ (point)))
                                 (selected-window)) window-cands)))
              (setq candidates (nconc candidates window-cands))))))
      (avy-process candidates))))

(defun avy-zh-goto-subword-1 (char &optional arg)
  "`avy-zh' replacement of `avy-goto-subword-1'.

Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-subword-1
    (let* ((char (downcase char))
           (chinese-regexp (zh-lib-build-regexp-char char t)))
      (avy-zh-goto-subword-0
       arg
       (lambda ()
         (or (and (char-after) (eq (downcase (char-after)) char))
             (string-match-p chinese-regexp (string (char-after)))))))))

(defun avy-zh-goto-word-or-subword-1 ()
  "`avy-zh' replacement of `avy-goto-word-or-subword-1'.

Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'."
  (interactive)
  (if (bound-and-true-p subword-mode)
      (call-interactively #'avy-zh-goto-subword-1)
    (call-interactively #'avy-zh-goto-word-1)))

;;;###autoload
(define-minor-mode avy-zh-mode
  "Jump to ZHongwen using `avy'."
  :init-value nil
  :lighter " Avy-ZH"
  :group 'avy-zh
  (if avy-zh-mode
      (progn
        (fset 'avy-goto-char #'avy-zh-goto-char)
        (fset 'avy-goto-char-2 #'avy-zh-goto-char-2)
        (fset 'avy-goto-char-in-line #'avy-zh-goto-char-in-line)
        (when avy-zh-treat-word-as-char
          (fset 'avy-goto-word-0 #'avy-zh-goto-word-0)
          (fset 'avy-goto-word-1 #'avy-zh-goto-word-1)
          (fset 'avy-goto-subword-0 #'avy-zh-goto-subword-0)
          (fset 'avy-goto-subword-1 #'avy-zh-goto-subword-1)
          (fset 'avy-goto-word-or-subword-1 #'avy-zh-goto-word-or-subword-1)))
    (progn
      (fset 'avy-goto-char avy-zh--original-avy-goto-char)
      (fset 'avy-goto-char-2 avy-zh--original-avy-goto-char-2)
      (fset 'avy-goto-char-in-line avy-zh--original-avy-goto-char-in-line)
      (fset 'avy-goto-word-0 avy-zh--original-avy-goto-word-0)
      (fset 'avy-goto-word-1 avy-zh--original-avy-goto-word-1)
      (fset 'avy-goto-subword-0 avy-zh--original-avy-goto-subword-0)
      (fset 'avy-goto-subword-1 avy-zh--original-avy-goto-subword-1)
      (fset 'avy-goto-word-or-subword-1 avy-zh--original-avy-goto-subword-1))))

;;;###autoload
(define-globalized-minor-mode global-avy-zh-mode
  avy-zh-mode
  turn-on-avy-zh-mode
  :group 'avy-zh
  :require 'avy-zh)

;;;###autoload
(defun turn-on-avy-zh-mode ()
  "Turn on `avy-zh-mode'."
  (interactive)
  (avy-zh-mode +1))

;;;###autoload
(defun turn-off-avy-zh-mode ()
  "Turn off `avy-zh-mode'."
  (interactive)
  (avy-zh-mode -1))

(provide 'avy-zh)

;;; avy-zh.el ends here
