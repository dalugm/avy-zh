;;; avy-zh-tests.el --- Tests for avy-zh -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'avy-zh)

(ert-deftest avy-zh-goto-char-uses-command-settings ()
  (let (seen-command)
    (cl-letf (((symbol-function 'avy-jump)
               (lambda (&rest _args)
                 (setq seen-command avy-command))))
      (avy-zh-goto-char ?a)
      (should (eq seen-command 'avy-goto-char)))))

(ert-deftest avy-zh-override-preserves-command-interface ()
  (let ((interactive-form-before (interactive-form #'avy-goto-char))
        seen-regexp)
    (unwind-protect
        (progn
          (avy-zh-mode 1)
          (should (commandp #'avy-goto-char))
          (should (equal (interactive-form #'avy-goto-char)
                         interactive-form-before))
          (cl-letf (((symbol-function 'avy-jump)
                     (lambda (regexp &rest _args)
                       (setq seen-regexp regexp))))
            (avy-goto-char ?a)
            (should (string-match-p seen-regexp "阿"))))
      (avy-zh-mode -1))))

(ert-deftest avy-zh-goto-word-0-forwards-scope ()
  (let (seen)
    (avy-zh-goto-word-0
     (lambda (&rest args)
       (setq seen (list args avy-goto-word-0-regexp)))
     '(4) 10 20)
    (should (equal (car seen) '((4) 10 20)))
    (should (equal (cadr seen) "\\b\\sw\\|\\cc"))))

(ert-deftest avy-zh-goto-subword-1-finds-each-chinese-character ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "阿八")
      (let (candidates)
        (unwind-protect
            (progn
              (avy-zh-mode 1)
              (cl-letf (((symbol-function 'avy-process)
                         (lambda (items)
                           (setq candidates items))))
                (avy-goto-subword-1 ?b))
              (should (equal (mapcar #'caar candidates) '(2))))
          (avy-zh-mode -1))))))

(ert-deftest avy-zh-mode-removes-all-advices ()
  (unwind-protect
      (progn
        (avy-zh-mode 1)
        (should (advice-member-p #'avy-zh-goto-subword-1
                                 'avy-goto-subword-1)))
    (avy-zh-mode -1))
  (dolist (pair '((avy-goto-char . avy-zh-goto-char)
                  (avy-goto-char-2 . avy-zh-goto-char-2)
                  (avy-goto-char-in-line . avy-zh-goto-char-in-line)
                  (avy-goto-char-timer . avy-zh-goto-char-timer)
                  (avy-goto-word-0 . avy-zh-goto-word-0)
                  (avy-goto-word-1 . avy-zh-goto-word-1)
                  (avy-goto-subword-0 . avy-zh-goto-subword-0)
                  (avy-goto-subword-1 . avy-zh-goto-subword-1)))
    (should-not (advice-member-p (cdr pair) (car pair)))))

(provide 'avy-zh-tests)
;;; avy-zh-tests.el ends here
