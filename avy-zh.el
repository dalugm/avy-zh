;;; avy-zh.el -- Jump to Chinese characters using `avy' -*- lexical-binding: t -*-

;; Author: dalu <mou.tong@qq.com>
;; Maintainer: dalu <mou.tong@qq.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1") (avy "0.4.0") (zh-lib "0.1.0"))
;; URL: https://github.com/dalugm/evil-zh
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

;;; Code:

(require 'avy)
(require 'zh-lib)

(defgroup avy-zh nil
  "Jump to Chinese characters using `avy'."
  :group 'avy)

(defcustom avy-zh-treat-word-as-char t
  "Whether word related `avy-*' commands should be remapped."
  :type 'boolean
  :group 'avy-zh)

(defun avy-zh-goto-char (orig-fn char &optional arg)
  "`avy-zh' version of `avy-goto-char'.

Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (avy-with orig-fn
    (avy-jump (if (eq char ?\C-m)
                  "\n"
                (zh-lib-build-regexp-char
                 char
                 (not zh-lib-with-punctuation)))
              :window-flip arg)))

(defun avy-zh-goto-char-in-line (orig-fn char)
  "`avy-zh' version of `avy-goto-char-in-line'.

Jump to the currently visible CHAR in the current line."
  (avy-with orig-fn
    (avy-jump (zh-lib-build-regexp-char
               char
               (not zh-lib-with-punctuation))
              :beg (line-beginning-position)
              :end (line-end-position))))

(defun avy-zh-goto-char-2 (orig-fn char1 char2 &optional arg beg end)
  "`avy-zh' version of `avy-goto-char-2'.

Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (when (eq char1 ?\C-m)
    (setq char1 ?\C-j))
  (when (eq char2 ?\C-m)
    (setq char2 ?\C-j))
  (avy-with orig-fn
    (avy-jump (zh-lib-build-regexp-string
               (string char1 char2)
               (not zh-lib-with-punctuation))
              :window-flip arg
              :beg beg
              :end end)))

(defun avy-zh-goto-char-timer (orig-fn &optional arg)
  "`avy-zh' version of `avy-goto-char-timer'.

Read one or many consecutive chars and jump to the first one.  The
window scope is determined by `avy-all-windows' (ARG negates it)."
  (let ((avy-all-windows (if arg
                             (not avy-all-windows)
                           avy-all-windows)))
    (avy-with orig-fn
      (setq avy--old-cands (avy--read-candidates 'zh-lib-build-regexp-string))
      (avy-process avy--old-cands))))

(defun avy-zh-goto-word-0 (orig-fn arg)
  "`avy-zh' version of `avy-goto-word-0'.

Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (let ((avy-goto-word-0-regexp "\\b\\sw\\|\\cc"))
    (funcall orig-fn arg)))

(defun avy-zh-goto-word-1 (orig-fn char &optional arg beg end symbol)
  "`avy-zh' version of `avy-goto-word-1'.

Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start."
  (avy-with orig-fn
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
(defvar subword-backward-regexp)

(defun avy-zh-goto-subword-0 (orig-fn &optional arg predicate beg end)
  "`avy-zh' version of `avy-goto-subword-0'.

Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it’s a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched."
  (require 'subword)
  (avy-with orig-fn
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
                                  (selected-window))
                            window-cands)))
                  (subword-backward))
                (and (= (point) ws)
                     (or (null predicate)
                         (and predicate (funcall predicate)))
                     (not (get-char-property (point) 'invisible))
                     (push (cons (cons (point) (1+ (point)))
                                 (selected-window))
                           window-cands)))
              (setq candidates (nconc candidates window-cands))))))
      (avy-process candidates))))

(defun avy-zh-goto-subword-1 (orig-fn char &optional arg)
  "`avy-zh' version of `avy-goto-subword-1'.

Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored."
  (avy-with orig-fn
    (let* ((char (downcase char))
           (chinese-regexp (zh-lib-build-regexp-char char t)))
      (avy-goto-subword-0
       arg
       (lambda ()
         (or (and (char-after) (eq (downcase (char-after)) char))
             (string-match-p chinese-regexp (string (char-after)))))))))

(defun avy-zh-goto-word-or-subword-1 (orig-fn)
  "`avy-zh' version of `avy-goto-word-or-subword-1'.

Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'."
  (if (bound-and-true-p subword-mode)
      (call-interactively #'avy-goto-subword-1)
    (call-interactively #'avy-goto-word-1)))

;;;###autoload
(define-minor-mode avy-zh-mode
  "Jump to ZHongwen using `avy'."
  :group 'avy-zh :global t
  (if avy-zh-mode
      (progn
        (advice-add 'avy-goto-char         :around #'avy-zh-goto-char)
        (advice-add 'avy-goto-char-2       :around #'avy-zh-goto-char-2)
        (advice-add 'avy-goto-char-in-line :around #'avy-zh-goto-char-in-line)
        (advice-add 'avy-goto-char-timer   :around #'avy-zh-goto-char-timer)
        (when avy-zh-treat-word-as-char
          (advice-add 'avy-goto-word-0            :around #'avy-zh-goto-word-0)
          (advice-add 'avy-goto-word-1            :around #'avy-zh-goto-word-1)
          (advice-add 'avy-goto-subword-0         :around #'avy-zh-goto-subword-0)
          (advice-add 'avy-goto-subword-1         :around #'avy-zh-goto-subword-1)
          (advice-add 'avy-goto-word-or-subword-1 :around #'avy-zh-goto-word-or-subword-1)))
    (progn
      (advice-remove 'avy-goto-char              #'avy-zh-goto-char)
      (advice-remove 'avy-goto-char-2            #'avy-zh-goto-char-2)
      (advice-remove 'avy-goto-char-in-line      #'avy-zh-goto-char-in-line)
      (advice-remove 'avy-goto-char-timer        #'avy-zh-goto-char-timer)
      (advice-remove 'avy-goto-word-0            #'avy-zh-goto-word-0)
      (advice-remove 'avy-goto-word-1            #'avy-zh-goto-word-1)
      (advice-remove 'avy-goto-subword-0         #'avy-zh-goto-subword-0)
      (advice-remove 'avy-goto-subword-1         #'avy-zh-goto-subword-1)
      (advice-remove 'avy-goto-word-or-subword-1 #'avy-zh-goto-subword-1))))

(provide 'avy-zh)
;;; avy-zh.el ends here
