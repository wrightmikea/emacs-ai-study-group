;;; test-correction.el --- Test the auto-correction function

;; Load the test file
(load-file "test-automated-ollama.el")

;; Test data from the actual AI response
(defvar test-ai-code
  "(defun fizzbuzz (n)
  (dotimes (i n)
    (if (or (zerop i) (and (zerop i % 3) (zerop i % 5)))
        (message \"FizzBuzz\")
      (format t \"%d\" i))))")

(message "=== ORIGINAL AI CODE ===")
(message "%s" test-ai-code)

(message "\n=== CORRECTED CODE ===")
(let ((corrected (test-automated-correct-code test-ai-code)))
  (message "%s" corrected)

  (message "\n=== TESTING CORRECTED CODE ===")
  (condition-case err
      (progn
        (eval (read corrected))
        (message "✓ Code evaluated successfully!")

        (message "\n=== RUNNING fizzbuzz(15) ===")
        (fizzbuzz 15)
        (message "✓ Executed without errors!"))
    (error
     (message "✗ Error: %s" (error-message-string err)))))

;;; test-correction.el ends here
