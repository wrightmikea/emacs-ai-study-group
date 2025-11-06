;;; test-correction-simple.el --- Test auto-correction standalone

;; Just the correction function
(defun test-automated-correct-code (code)
  "Generate a corrected version of CODE."
  (let ((corrected code))
    ;; Fix infix % syntax: "i % N" → "(mod i N)"
    (setq corrected (replace-regexp-in-string
                     "\\([a-z]+\\) % \\([0-9]+\\)"
                     "(mod \\1 \\2)"
                     corrected))
    ;; Fix %N syntax in comparisons
    (setq corrected (replace-regexp-in-string
                     "(< \\([a-z]+\\) %\\([0-9]+\\))"
                     "(zerop (mod \\1 \\2))"
                     corrected))
    (setq corrected (replace-regexp-in-string
                     "(= \\([a-z]+\\) %\\([0-9]+\\))"
                     "(zerop (mod \\1 \\2))"
                     corrected))
    ;; Fix format t
    (setq corrected (replace-regexp-in-string
                     "format t"
                     "message"
                     corrected))
    corrected))

;; Test with actual AI-generated code
(defvar test-ai-code
  "(defun fizzbuzz (n)
  (dotimes (i n)
    (if (or (zerop i) (and (zerop i % 3) (zerop i % 5)))
        (message \"FizzBuzz\")
      (format t \"%d\" i))))")

(message "=== ORIGINAL AI CODE ===")
(message "%s\n" test-ai-code)

(let ((corrected (test-automated-correct-code test-ai-code)))
  (message "=== CORRECTED CODE ===")
  (message "%s\n" corrected)

  (message "=== TESTING CORRECTED CODE ===")
  (condition-case err
      (progn
        (eval (read corrected))
        (message "✓ Code evaluated successfully!\n")
        (message "=== RUNNING fizzbuzz(15) ===")
        (fizzbuzz 15)
        (message "\n✓ Function executed without errors!"))
    (error
     (message "✗ Error: %s" (error-message-string err)))))

;;; test-correction-simple.el ends here
