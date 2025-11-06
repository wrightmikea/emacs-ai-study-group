;;; test-ai-iterative.el --- Test AI with progressively better prompts -*- lexical-binding: t; -*-

;; Tests ONLY what AI actually generates - NO auto-correction
;; Shows whether better prompts lead to better AI code

;;; Code:

(require 'gptel)
(require 'cl-lib)

(defvar test-ai-iterative-results-dir
  (expand-file-name "results" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory for test results.")

(defvar test-ai-iterative-current-attempt 0
  "Current attempt number.")

(defvar test-ai-iterative-prompts
  '((1 . "Write a FizzBuzz function.")

    (2 . "Write a FizzBuzz function in Emacs Lisp.")

    (3 . "Write an Emacs Lisp function called fizzbuzz.
Use dotimes for iteration from 1 to n.
Use message for output.
Print Fizz for multiples of 3, Buzz for multiples of 5, FizzBuzz for multiples of both.")

    (4 . "Write ONLY an Emacs Lisp function called fizzbuzz.
Do NOT add defgroup, defcustom, or provide.
Use dotimes to iterate from 1 to n.
Use message for output (NOT format t).
This is EMACS LISP, not Common Lisp.
Check divisibility using (zerop (mod num 15)) for FizzBuzz, (mod num 3) for Fizz, (mod num 5) for Buzz.")

    (5 . "Write ONLY an Emacs Lisp function called fizzbuzz.
Do NOT use: format t, loop, defparameter, defgroup, defcustom, provide.
DO use: dotimes, message, zerop, mod, cond.
Take argument n (integer).
For each number 1 to n:
  - If divisible by 15: (message \"FizzBuzz\")
  - Else if divisible by 3: (message \"Fizz\")
  - Else if divisible by 5: (message \"Buzz\")
  - Else: (message \"%d\" num)
Remember: dotimes starts at 0, use (1+ i) for 1-based.
This is EMACS LISP (not Common Lisp).
Return only: (defun fizzbuzz (n) ...body...)"))
  "Progressive prompt improvements - from vague to very explicit.")

;;;; Expected output verification

(defvar test-ai-expected-fizzbuzz-15
  '("1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz")
  "Expected output for fizzbuzz(15).")

(defun test-ai-verify-fizzbuzz-output (output-string)
  "Verify OUTPUT-STRING matches expected FizzBuzz(15) output.
Returns (correct-p differences)."
  (let* ((lines (split-string output-string "\n" t))
         (expected test-ai-expected-fizzbuzz-15)
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
      (let ((got (nth i lines))
            (exp (nth i expected)))
        (unless (string= got exp)
          (push (format "Line %d: got \"%s\", expected \"%s\""
                       (1+ i) got exp)
               differences)
          (setq correct-p nil))))

    (list correct-p (nreverse differences))))

;;;; Code extraction

(defun test-ai-extract-code (response)
  "Extract elisp code from RESPONSE - handles markdown and plain text."
  (cond
   ;; Markdown code block with language
   ((string-match "```\\(?:elisp\\|emacs-lisp\\|lisp\\)\n\\(\\(?:.\\|\n\\)*?\\)```" response)
    (match-string 1 response))
   ;; Generic code block
   ((string-match "```\n\\(\\(?:.\\|\n\\)*?\\)```" response)
    (match-string 1 response))
   ;; Look for defun
   ((string-match "(defun fizzbuzz[^(]*([^)]*)" response)
    (let ((start (match-beginning 0)))
      (with-temp-buffer
        (insert response)
        (goto-char (1+ start))
        (condition-case nil
            (progn
              (forward-sexp)
              (buffer-substring start (point)))
          (error
           ;; If sexp fails, return whole response
           response)))))
   ;; Return as-is
   (t response)))

;;;; Code analysis (detection only - NO correction)

(defun test-ai-analyze-mistakes (code)
  "Analyze CODE for mistakes - detection only, NO correction.
Returns list of mistake descriptions."
  (let ((mistakes '()))

    ;; Common Lisp syntax
    (when (string-match-p "format t" code)
      (push "Uses 'format t' (Common Lisp - will fail in Emacs)" mistakes))

    (when (string-match-p "(loop\\s-+for" code)
      (push "Uses 'loop for' (Common Lisp - will fail in Emacs)" mistakes))

    (when (string-match-p "defparameter" code)
      (push "Uses 'defparameter' (Common Lisp - not standard Emacs)" mistakes))

    ;; Invalid modulo syntax
    (when (string-match-p "[a-z] % [0-9]" code)
      (push "Uses infix % operator (C/Python syntax - not valid in Lisp)" mistakes))

    (when (string-match-p "%[0-9]" code)
      (push "Uses %N syntax (invalid in Emacs Lisp)" mistakes))

    ;; Unwanted additions
    (when (string-match-p "defgroup" code)
      (push "Added unnecessary 'defgroup' (not requested)" mistakes))

    (when (string-match-p "defcustom" code)
      (push "Added unnecessary 'defcustom' (not requested)" mistakes))

    (when (string-match-p "provide '" code)
      (push "Added unnecessary 'provide' (not requested)" mistakes))

    ;; Missing essentials
    (unless (string-match-p "(defun fizzbuzz" code)
      (push "Missing 'defun fizzbuzz' definition" mistakes))

    mistakes))

;;;; Code testing

(defun test-ai-test-code (code)
  "Test CODE - evaluate, run, verify output.
Returns (eval-ok eval-error run-ok run-error output verified-ok differences).
Tests ONLY what AI generated - NO corrections."
  (let ((eval-ok nil)
        (eval-error nil)
        (run-ok nil)
        (run-error nil)
        (output nil)
        (verified-ok nil)
        (differences nil))

    ;; Step 1: Try to evaluate the function definition
    (condition-case err
        (progn
          (eval (read code))
          (setq eval-ok t))
      (error
       (setq eval-error (error-message-string err))))

    ;; Step 2: If eval worked, try to run it
    (when eval-ok
      (condition-case err
          (progn
            ;; Capture messages by advising the message function
            (let ((messages '()))
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (push (apply #'format format-string args) messages))))
                (fizzbuzz 15))
              (setq output (mapconcat #'identity (nreverse messages) "\n")))
            (setq run-ok t))
        (error
         (setq run-error (error-message-string err)))))

    ;; Step 3: If run worked, verify output is correct
    (when (and run-ok output)
      (let ((verification (test-ai-verify-fizzbuzz-output output)))
        (setq verified-ok (nth 0 verification))
        (setq differences (nth 1 verification))))

    (list eval-ok eval-error run-ok run-error output verified-ok differences)))

;;;; Result saving

(defun test-ai-save-attempt (attempt prompt response)
  "Save ATTEMPT results with PROMPT and RESPONSE.
Documents ONLY what AI generated - NO corrections applied."
  (unless (file-directory-p test-ai-iterative-results-dir)
    (make-directory test-ai-iterative-results-dir t))

  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "%s/ai-attempt-%d-%s.txt"
                          test-ai-iterative-results-dir
                          attempt
                          timestamp))
         (code (test-ai-extract-code response))
         (mistakes (test-ai-analyze-mistakes code))
         (test-result (test-ai-test-code code))
         (eval-ok (nth 0 test-result))
         (eval-err (nth 1 test-result))
         (run-ok (nth 2 test-result))
         (run-err (nth 3 test-result))
         (output (nth 4 test-result))
         (verified (nth 5 test-result))
         (diffs (nth 6 test-result)))

    (with-temp-file filename
      (insert "===================================================================\n")
      (insert (format "AI ITERATIVE TEST - Attempt %d of %d\n"
                     attempt (length test-ai-iterative-prompts)))
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
      (insert "AI RESPONSE (EXACTLY AS RECEIVED)\n")
      (insert "-------------------------------------------------------------------\n")
      (insert response)
      (insert "\n\n")

      (insert "-------------------------------------------------------------------\n")
      (insert "EXTRACTED CODE (NO MODIFICATIONS)\n")
      (insert "-------------------------------------------------------------------\n")
      (insert code)
      (insert "\n\n")

      (insert "-------------------------------------------------------------------\n")
      (insert "MISTAKE ANALYSIS\n")
      (insert "-------------------------------------------------------------------\n")
      (if mistakes
          (progn
            (insert (format "Status: ✗ %d MISTAKE(S) DETECTED\n\n" (length mistakes)))
            (dolist (mistake mistakes)
              (insert (format "  ✗ %s\n" mistake))))
        (insert "Status: ✓ No obvious mistakes detected\n"))
      (insert "\n")

      (insert "-------------------------------------------------------------------\n")
      (insert "TEST 1: EVALUATION (Can the code be loaded?)\n")
      (insert "-------------------------------------------------------------------\n")
      (if eval-ok
          (insert "✓ SUCCESS - Code evaluated without errors\n")
        (insert (format "✗ FAILED - Code failed to evaluate\n   Error: %s\n" eval-err)))
      (insert "\n")

      (insert "-------------------------------------------------------------------\n")
      (insert "TEST 2: EXECUTION (Can the function run?)\n")
      (insert "-------------------------------------------------------------------\n")
      (if (not eval-ok)
          (insert "⊘ SKIPPED - Code did not evaluate\n")
        (if run-ok
            (insert "✓ SUCCESS - Function executed without errors\n")
          (insert (format "✗ FAILED - Function crashed during execution\n   Error: %s\n" run-err))))
      (insert "\n")

      (insert "-------------------------------------------------------------------\n")
      (insert "TEST 3: OUTPUT VERIFICATION (Is the output correct?)\n")
      (insert "-------------------------------------------------------------------\n")
      (if (not run-ok)
          (insert "⊘ SKIPPED - Function did not run successfully\n")
        (progn
          (insert "Output received:\n")
          (insert output)
          (insert "\n")
          (if verified
              (insert "✓ SUCCESS - Output matches expected FizzBuzz(15) sequence!\n")
            (progn
              (insert "✗ FAILED - Output does not match expected sequence\n\n")
              (insert "Expected output:\n")
              (dolist (exp test-ai-expected-fizzbuzz-15)
                (insert (format "  %s\n" exp)))
              (insert "\nDifferences found:\n")
              (dolist (diff diffs)
                (insert (format "  ✗ %s\n" diff)))))))
      (insert "\n")

      (insert "-------------------------------------------------------------------\n")
      (insert "SUMMARY\n")
      (insert "-------------------------------------------------------------------\n")
      (insert (format "Prompt Quality: Attempt %d (vague → explicit)\n" attempt))
      (insert (format "Evaluation: %s\n" (if eval-ok "PASS" "FAIL")))
      (insert (format "Execution: %s\n" (if run-ok "PASS" (if eval-ok "FAIL" "N/A"))))
      (insert (format "Output Correct: %s\n" (if verified "PASS" (if run-ok "FAIL" "N/A"))))
      (insert (format "Mistakes Detected: %d\n" (length mistakes)))
      (insert (format "Overall: %s\n"
                     (cond
                      (verified "✓✓✓ FULLY WORKING - AI GENERATED VALID CODE!")
                      (run-ok "⚠ RUNS BUT OUTPUT WRONG")
                      (eval-ok "⚠ LOADS BUT CRASHES WHEN RUN")
                      (t "✗ COMPLETELY BROKEN"))))
      (insert "\n")
      (insert "===================================================================\n")
      (insert "NOTE: This shows ONLY what the AI generated.\n")
      (insert "NO automatic corrections were applied.\n")
      (insert "===================================================================\n"))

    (message "Saved attempt %d to: %s" attempt filename)
    filename))

;;;; Main test function

(defun test-ai-iterative-all ()
  "Run all progressive prompts through Ollama.
Tests ONLY what AI generates - NO corrections.
Shows whether better prompts lead to better AI code."
  (interactive)

  (unless (featurep 'gptel)
    (error "gptel not loaded. Run: (require 'gptel)"))

  (unless (boundp 'gptel-backend)
    (error "gptel-backend not configured. Load quick-setup.el first"))

  (setq test-ai-iterative-current-attempt 0)

  (message "\n===================================================================")
  (message "AI ITERATIVE PROMPT TEST")
  (message "===================================================================")
  (message "Testing %d progressive prompts (vague → explicit)" (length test-ai-iterative-prompts))
  (message "Backend: %s" (oref gptel-backend name))
  (message "Model: %s" gptel-model)
  (message "")
  (message "This tests ONLY what the AI generates.")
  (message "NO automatic corrections will be applied.")
  (message "===================================================================\n")

  ;; Start with attempt 1
  (test-ai-iterative--process-attempt 1))

(defun test-ai-iterative--process-attempt (attempt)
  "Process ATTEMPT number by sending prompt to AI."
  (if (> attempt (length test-ai-iterative-prompts))
      (progn
        (message "\n===================================================================")
        (message "ALL ATTEMPTS COMPLETE")
        (message "===================================================================")
        (message "Processed %d prompts" (length test-ai-iterative-prompts))
        (message "Results saved to: %s" test-ai-iterative-results-dir)
        (message "\nCompare results to see if better prompts → better AI code!")
        (message "==================================================================="))

    (let* ((prompt-info (assoc attempt test-ai-iterative-prompts))
           (prompt (cdr prompt-info)))

      (message "\n--- Attempt %d/%d ---" attempt (length test-ai-iterative-prompts))
      (message "Sending prompt to Ollama...")

      (gptel-request prompt
        :callback
        (lambda (response info)
          (if (not (stringp response))
              (progn
                (message "✗ Attempt %d: No valid response received" attempt)
                (test-ai-save-attempt attempt prompt "ERROR: No response")
                ;; Continue to next
                (run-with-timer 2 nil #'test-ai-iterative--process-attempt (1+ attempt)))

            (message "✓ Attempt %d: Response received (%d chars)" attempt (length response))
            (test-ai-save-attempt attempt prompt response)

            ;; Continue to next attempt after delay
            (run-with-timer 3 nil #'test-ai-iterative--process-attempt (1+ attempt))))))))

(provide 'test-ai-iterative)
;;; test-ai-iterative.el ends here
