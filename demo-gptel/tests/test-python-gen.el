;;; test-python-gen.el --- Test AI Python code generation via gptel -*- lexical-binding: t; -*-

;; Test that uses gptel to ask AI to write Python code, then runs and verifies it

;;; Code:

(require 'gptel)

(defvar test-python-results-dir
  (expand-file-name "results" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory for test results.")

(defvar test-python-expected-fizzbuzz
  '("1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz")
  "Expected FizzBuzz output for n=15.")

;;;; Code extraction

(defun test-python-extract-code (response)
  "Extract Python code from RESPONSE."
  (cond
   ;; Python code block
   ((string-match "```python\n\\(\\(?:.\\|\n\\)*?\\)```" response)
    (match-string 1 response))
   ;; Generic code block
   ((string-match "```\n\\(\\(?:.\\|\n\\)*?\\)```" response)
    (match-string 1 response))
   ;; Look for def fizzbuzz
   ((string-match "def fizzbuzz.*" response)
    (let ((start (match-beginning 0)))
      ;; Extract from def to end of response
      (substring response start)))
   ;; Return as-is
   (t response)))

;;;; Python execution

(defun test-python-run-code (code)
  "Write CODE to temp file and run it with Python.
Returns (success-p output error)."
  (let* ((temp-file (make-temp-file "test-python-" nil ".py"))
         (test-code (format "%s\n\n# Test it\nfizzbuzz(15)\n" code))
         (output nil)
         (error-output nil)
         (success-p nil))

    ;; Write code to file
    (with-temp-file temp-file
      (insert test-code))

    ;; Run it
    (with-temp-buffer
      (let ((exit-code (call-process "python3" nil (current-buffer) nil temp-file)))
        (if (= exit-code 0)
            (progn
              (setq output (buffer-string))
              (setq success-p t))
          (setq error-output (buffer-string)))))

    ;; Clean up
    (delete-file temp-file)

    (list success-p output error-output)))

;;;; Output verification

(defun test-python-verify-output (output)
  "Verify OUTPUT matches expected FizzBuzz.
Returns (correct-p differences)."
  (let* ((lines (split-string (string-trim output) "\n" t))
         (expected test-python-expected-fizzbuzz)
         (differences '())
         (correct-p t))

    ;; Check length
    (unless (= (length lines) (length expected))
      (push (format "Wrong number of outputs: got %d, expected %d"
                   (length lines) (length expected))
           differences)
      (setq correct-p nil))

    ;; Check each line
    (dotimes (i (min (length lines) (length expected)))
      (let ((got (string-trim (nth i lines)))
            (exp (nth i expected)))
        (unless (string= got exp)
          (push (format "Line %d: got \"%s\", expected \"%s\""
                       (1+ i) got exp)
               differences)
          (setq correct-p nil))))

    (list correct-p (nreverse differences))))

;;;; Result logging

(defun test-python-save-result (prompt response code run-result verify-result)
  "Save test results to log file."
  (unless (file-directory-p test-python-results-dir)
    (make-directory test-python-results-dir t))

  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "%s/python-test-%s.txt" test-python-results-dir timestamp))
         (run-ok (nth 0 run-result))
         (output (nth 1 run-result))
         (error (nth 2 run-result))
         (verified (nth 0 verify-result))
         (diffs (nth 1 verify-result)))

    (with-temp-file filename
      (insert "===================================================================\n")
      (insert "PYTHON CODE GENERATION TEST\n")
      (insert "===================================================================\n\n")
      (insert (format "Timestamp: %s\n" (current-time-string)))
      (insert (format "Backend: %s\n" (oref gptel-backend name)))
      (insert (format "Model: %s\n\n" gptel-model))

      (insert "-------------------------------------------------------------------\n")
      (insert "PROMPT SENT TO AI\n")
      (insert "-------------------------------------------------------------------\n")
      (insert prompt)
      (insert "\n\n")

      (insert "-------------------------------------------------------------------\n")
      (insert "AI RESPONSE\n")
      (insert "-------------------------------------------------------------------\n")
      (insert response)
      (insert "\n\n")

      (insert "-------------------------------------------------------------------\n")
      (insert "EXTRACTED PYTHON CODE\n")
      (insert "-------------------------------------------------------------------\n")
      (insert code)
      (insert "\n\n")

      (insert "-------------------------------------------------------------------\n")
      (insert "EXECUTION TEST\n")
      (insert "-------------------------------------------------------------------\n")
      (if run-ok
          (progn
            (insert "✓ Code executed successfully\n\n")
            (insert "Output:\n")
            (insert output)
            (insert "\n"))
        (progn
          (insert "✗ Code failed to execute\n\n")
          (insert "Error:\n")
          (insert (or error "Unknown error"))
          (insert "\n")))

      (when run-ok
        (insert "-------------------------------------------------------------------\n")
        (insert "OUTPUT VERIFICATION\n")
        (insert "-------------------------------------------------------------------\n")
        (if verified
            (insert "✓✓✓ SUCCESS - Output matches expected FizzBuzz sequence!\n")
          (progn
            (insert "✗ Output does not match expected\n\n")
            (insert "Differences:\n")
            (dolist (diff diffs)
              (insert (format "  %s\n" diff))))))

      (insert "\n")
      (insert "-------------------------------------------------------------------\n")
      (insert "SUMMARY\n")
      (insert "-------------------------------------------------------------------\n")
      (insert (format "Execution: %s\n" (if run-ok "PASS" "FAIL")))
      (insert (format "Verification: %s\n" (if verified "PASS" (if run-ok "FAIL" "N/A"))))
      (insert (format "Overall: %s\n"
                     (cond
                      (verified "✓✓✓ FULLY WORKING")
                      (run-ok "⚠ RUNS BUT OUTPUT WRONG")
                      (t "✗ FAILED TO RUN"))))
      (insert "\n===================================================================\n"))

    (message "Results saved to: %s" filename)
    filename))

;;;; Main test function

(defun test-python-fizzbuzz ()
  "Ask AI to generate Python FizzBuzz, run it, and verify results.
This demonstrates gptel calling Ollama to generate working code."
  (interactive)

  (unless (featurep 'gptel)
    (error "gptel not loaded"))

  (unless (boundp 'gptel-backend)
    (error "gptel-backend not configured"))

  (let ((prompt "Write a Python function called fizzbuzz(n) that prints the FizzBuzz sequence from 1 to n.
For each number:
- If divisible by 15: print 'FizzBuzz'
- Else if divisible by 3: print 'Fizz'
- Else if divisible by 5: print 'Buzz'
- Else: print the number

Return ONLY the function definition, no examples or explanations."))

    (message "Asking AI to generate Python FizzBuzz...")
    (message "Backend: %s, Model: %s" (oref gptel-backend name) gptel-model)

    (gptel-request prompt
      :callback
      (lambda (response info)
        (if (not (stringp response))
            (message "Error: No response from AI")

          (message "✓ Response received (%d chars)" (length response))

          (let* ((code (test-python-extract-code response))
                 (run-result (test-python-run-code code))
                 (verify-result (when (nth 0 run-result)
                                 (test-python-verify-output (nth 1 run-result)))))

            (test-python-save-result prompt response code run-result verify-result)

            (if (and (nth 0 run-result) (nth 0 verify-result))
                (message "\n✓✓✓ SUCCESS! AI generated working Python FizzBuzz!")
              (message "\n✗ Test failed - see results file for details"))

            (message "Check: %s" test-python-results-dir)))))))

(provide 'test-python-gen)
;;; test-python-gen.el ends here
