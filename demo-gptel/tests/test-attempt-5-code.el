;;; test-attempt-5-code.el --- Test the actual code from attempt 5

(require 'cl-lib)

;; This is the EXACT code the AI generated in attempt 5
(defun fizzbuzz (n)
  (dotimes (i (1- n))
    (cond ((zerop (mod i 15)) (message "FizzBuzz"))
          ((zerop (mod i 3)) (message "Fizz"))
          ((zerop (mod i 5)) (message "Buzz"))
          (t (message "%d" (1+ i)))))
  nil)

;; Test it with proper message capturing
(message "\n=== Testing AI-generated code from Attempt 5 ===\n")

(let ((messages '()))
  (cl-letf (((symbol-function 'message)
             (lambda (format-string &rest args)
               (push (apply #'format format-string args) messages))))
    (fizzbuzz 15))
  (let ((output (mapconcat #'identity (nreverse messages) "\n")))
    (message "Output captured:\n%s\n" output)
    (message "Number of lines: %d" (length (split-string output "\n" t)))
    (message "\nExpected: 15 lines (1, 2, Fizz, 4, Buzz, Fizz, 7, 8, Fizz, Buzz, 11, Fizz, 13, 14, FizzBuzz)")
    (message "Does it match? Let's see...\n")

    ;; Check expected values
    (let ((lines (split-string output "\n" t))
          (expected '("1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz")))
      (if (equal lines expected)
          (message "✓✓✓ SUCCESS - Output matches perfectly!")
        (progn
          (message "✗ Output doesn't match")
          (message "\nGot:")
          (dolist (line lines)
            (message "  %s" line))
          (message "\nExpected:")
          (dolist (exp expected)
            (message "  %s" exp)))))))

;;; test-attempt-5-code.el ends here
