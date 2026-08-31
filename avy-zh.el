;;; avy-zh.el --- Jump to Chinese characters using `avy' -*- lexical-binding: t -*-

;; Author: dalu <mou.tong@qq.com>
;; Maintainer: dalu <mou.tong@qq.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "30.1") (avy "0.5.0") (zh-lib "0.2.0"))
;; URL: https://github.com/dalugm/avy-zh
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
(require 'subr-x)
(require 'zh-lib)

(defgroup avy-zh nil
  "Jump to Chinese characters using `avy'."
  :group 'avy)

(defcustom avy-zh-treat-word-as-char t
  "Whether word related `avy-*' commands should be remapped."
  :type 'boolean
  :group 'avy-zh)

(defun avy-zh-goto-char (char &optional arg)
  "`avy-zh' version of `avy-goto-char'.

Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (avy-with avy-goto-char
    (avy-jump (if (eq char ?\C-m)
                  "\n"
                (zh-lib-build-regexp char))
              :window-flip arg)))

(defun avy-zh-goto-char-in-line (char)
  "`avy-zh' version of `avy-goto-char-in-line'.

Jump to the currently visible CHAR in the current line."
  (avy-with avy-goto-char
    (avy-jump (zh-lib-build-regexp char)
              :beg (pos-bol)
              :end (pos-eol))))

(defun avy-zh-goto-char-2 (char1 char2 &optional arg beg end)
  "`avy-zh' version of `avy-goto-char-2'.

Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (when (eq char1 ?\C-m)
    (setq char1 ?\C-j))
  (when (eq char2 ?\C-m)
    (setq char2 ?\C-j))
  (avy-with avy-goto-char-2
    (avy-jump (zh-lib-build-regexp (string char1 char2))
              :window-flip arg
              :beg beg
              :end end)))

(defun avy-zh--read-timer-input ()
  "Read one or more characters, stopping after `avy-timeout-seconds'."
  (let ((text "")
        char
        done)
    (while (and (not done)
                (setq char
                      (read-char
                       (if (string-empty-p text)
                           "char: "
                         (format "char (%s): " text))
                       t
                       (and (not (string-empty-p text))
                            avy-timeout-seconds))))
      (cond
       ((eq char ?\C-m)
        (if avy-enter-times-out
            (setq done t)
          (setq text (concat text "\n"))))
       ((memq char avy-del-last-char-by)
        (unless (string-empty-p text)
          (setq text (substring text 0 -1))))
       ((eq char ?\e)
        (keyboard-quit))
       (t
        (setq text (concat text (string char))))))
    text))

(defun avy-zh-goto-char-timer (&optional arg)
  "`avy-zh' version of `avy-goto-char-timer'.

Read one or many consecutive chars and jump to the first one.  The
window scope is determined by `avy-all-windows' (ARG negates it)."
  (avy-with avy-goto-char-timer
    (setq avy-text (avy-zh--read-timer-input))
    (unless (string-empty-p avy-text)
      (avy-jump (zh-lib-build-regexp avy-text)
                :window-flip arg))))

(defun avy-zh-goto-word-0 (orig-fn arg &optional beg end)
  "`avy-zh' version of `avy-goto-word-0'.

Call ORIG-FN with ARG, BEG, and END after extending its word regexp.
Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (let ((avy-goto-word-0-regexp "\\b\\sw\\|\\cc"))
    (funcall orig-fn arg beg end)))

(defun avy-zh-goto-word-1 (char &optional arg beg end symbol)
  "`avy-zh' version of `avy-goto-word-1'.

Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start."
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
                     "\\|"
                     (zh-lib-build-regexp char))))))
      (avy-jump regex
                :window-flip arg
                :beg beg
                :end end))))

(declare-function subword-backward "subword" (&optional arg))
(defvar subword-backward-regexp)

(defun avy-zh-goto-subword-0 (&optional arg predicate beg end)
  "`avy-zh' version of `avy-goto-subword-0'.

Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it’s a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched."
  (require 'subword)
  (avy-with avy-goto-subword-0
    (let ((case-fold-search nil)
          (subword-backward-regexp
           (concat
            "\\(\\(\\W\\|[[:lower:][:digit:]]\\)"
            "\\([!-/:@`~[:upper:]]+\\W*\\)\\|\\W\\w+\\|.\\cc\\)"))
          candidates)
      (avy-dowindows arg
        (let ((syntax-table (copy-syntax-table)))
          (dolist (char avy-subword-extra-word-chars)
            (modify-syntax-entry char "w" syntax-table))
          (with-syntax-table syntax-table
            (let ((start (or beg (window-start)))
                  window-candidates)
              (save-excursion
                (goto-char (or end (window-end (selected-window) t)))
                (subword-backward)
                (while (> (point) start)
                  (when (and (or (null predicate)
                                 (funcall predicate))
                             (not (invisible-p (point))))
                    (push (cons (cons (point) (1+ (point)))
                                (selected-window))
                          window-candidates))
                  (subword-backward))
                (when (and (= (point) start)
                           (or (null predicate)
                               (funcall predicate))
                           (not (invisible-p (point))))
                  (push (cons (cons (point) (1+ (point)))
                              (selected-window))
                        window-candidates)))
              (setq candidates
                    (nconc candidates window-candidates))))))
      (avy-process candidates))))

(defun avy-zh-goto-subword-1 (char &optional arg)
  "`avy-zh' version of `avy-goto-subword-1'.

Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored."
  (avy-with avy-goto-subword-1
    (let* ((char (downcase char))
           (chinese-regexp
            (zh-lib-build-regexp char :with-punctuation nil)))
      (avy-goto-subword-0
       arg
       (lambda ()
         (let ((candidate (char-after)))
           (and candidate
                (or (eq (downcase candidate) char)
                    (string-match-p chinese-regexp
                                    (string candidate))))))))))

;;;###autoload
(define-minor-mode avy-zh-mode
  "Jump to ZHongwen using `avy'."
  :group 'avy-zh :global t
  (if avy-zh-mode
      (progn
        (advice-add 'avy-goto-char         :override #'avy-zh-goto-char)
        (advice-add 'avy-goto-char-2       :override #'avy-zh-goto-char-2)
        (advice-add 'avy-goto-char-in-line :override #'avy-zh-goto-char-in-line)
        (advice-add 'avy-goto-char-timer   :override #'avy-zh-goto-char-timer)
        (when avy-zh-treat-word-as-char
          (advice-add 'avy-goto-word-0            :around #'avy-zh-goto-word-0)
          (advice-add 'avy-goto-word-1            :override #'avy-zh-goto-word-1)
          (advice-add 'avy-goto-subword-0         :override #'avy-zh-goto-subword-0)
          (advice-add 'avy-goto-subword-1         :override #'avy-zh-goto-subword-1)))
    (advice-remove 'avy-goto-char              #'avy-zh-goto-char)
    (advice-remove 'avy-goto-char-2            #'avy-zh-goto-char-2)
    (advice-remove 'avy-goto-char-in-line      #'avy-zh-goto-char-in-line)
    (advice-remove 'avy-goto-char-timer        #'avy-zh-goto-char-timer)
    (advice-remove 'avy-goto-word-0            #'avy-zh-goto-word-0)
    (advice-remove 'avy-goto-word-1            #'avy-zh-goto-word-1)
    (advice-remove 'avy-goto-subword-0         #'avy-zh-goto-subword-0)
    (advice-remove 'avy-goto-subword-1         #'avy-zh-goto-subword-1)))

(provide 'avy-zh)
;;; avy-zh.el ends here
