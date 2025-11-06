;;; test-iterative-prompt-improvement.el --- Iterative AI prompt testing -*- lexical-binding: t; -*-

;; This demonstrates iterative prompt improvement until AI generates valid code

;;; Commentary:
;;
;; This test:
;; 1. Starts with a vague prompt
;; 2. Captures AI response
;; 3. Analyzes for mistakes
;; 4. If mistakes found, improves prompt and tries again
;; 5. Repeats until valid code is generated (or max attempts)
;; 6. Saves all attempts for review
;;
;; Run interactively:
;;   M-x load-file RET test-iterative-prompt-improvement.el RET
;;   M-x test-iterative-fizzbuzz RET
;;
;; Prerequisites:
;; - gptel loaded and configured
;; - Ollama running locally
;; - A model pulled (e.g., llama3.2, codellama)

;;; Code:

(require 'gptel nil t)

(defvar test-iterative-results-dir
  (expand-file-name "results" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory for results.")

(defvar test-iterative-max-attempts 5
  "Maximum number of prompt improvement attempts.")

(defvar test-iterative-current-attempt 0
  "Current attempt number.")

(defvar test-iterative-all-attempts '()
  "List of all attempts: (attempt-num prompt response analysis).")

;;;; Prompt Improvement Strategy

(defvar test-iterative-prompts
  '((1 . "Write a FizzBuzz function.")

    (2 . "Write a FizzBuzz function in Emacs Lisp.")

    (3 . "Write an Emacs Lisp function called fizzbuzz.
Use dotimes for iteration.
Use message for output.")

    (4 . "Write ONLY an Emacs Lisp function called fizzbuzz.
Do NOT add defgroup, defcustom, or provide.
Use dotimes to iterate from 1 to n.
Use message for output (NOT format t).
This is EMACS LISP, not Common Lisp.")

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
  "Progressive prompt improvements.")

;;;; Analysis Functions

(defun test-iterative-analyze-response (response)
  "Analyze response for mistakes and validity.
Returns (valid-p mistakes suggestions)."
  (let ((mistakes '())
        (suggestions '())
        (valid-p t))

    ;; Check for Common Lisp mistakes
    (when (string-match-p "format t" response)
      (push "Uses 'format t' (Common Lisp - won't work)" mistakes)
      (push "Change to (message ...) for Emacs Lisp" suggestions)
      (setq valid-p nil))

    (when (string-match-p "(loop\\s-+for" response)
      (push "Uses 'loop for' (Common Lisp - won't work)" mistakes)
      (push "Change to (dotimes ...) or (dolist ...)" suggestions)
      (setq valid-p nil))

    (when (string-match-p "defparameter" response)
      (push "Uses 'defparameter' (Common Lisp)" mistakes)
      (push "Change to (defvar ...) or (defconst ...)" suggestions)
      (setq valid-p nil))

    ;; Check for unwanted additions
    (when (string-match-p "defgroup" response)
      (push "Added unnecessary 'defgroup'" mistakes)
      (push "Remove defgroup - just provide function" suggestions))

    (when (string-match-p "defcustom" response)
      (push "Added unnecessary 'defcustom'" mistakes)
      (push "Remove defcustom - not needed" suggestions))

    ;; Check for correct elements
    (unless (string-match-p "(defun fizzbuzz" response)
      (push "Missing 'defun fizzbuzz' definition" mistakes)
      (push "Include (defun fizzbuzz (n) ...)" suggestions)
      (setq valid-p nil))

    ;; Check iteration (should have dotimes or dolist)
    (unless (or (string-match-p "dotimes" response)
                (string-match-p "dolist" response)
                (string-match-p "cl-loop" response)
                (string-match-p "while" response))
      (push "No iteration construct found" mistakes)
      (push "Add (dotimes (i n) ...)" suggestions)
      (setq valid-p nil))

    ;; Check output method
    (cond
     ((string-match-p "(message" response)
      ;; Good - has message
      nil)
     ((string-match-p "format t" response)
      ;; Already caught above
      nil)
     (t
      (push "No output mechanism found" mistakes)
      (push "Add (message ...) calls" suggestions)))

    (list valid-p mistakes suggestions)))

(defun test-iterative-extract-code (response)
  "Extract elisp code from response.
Handles code blocks and plain text."
  (cond
   ;; Markdown code block
   ((string-match "```\\(elisp\\|emacs-lisp\\|lisp\\)?\n\\(\\(?:.\\|\n\\)*?\\)\n```" response)
    (match-string 2 response))

   ;; Look for defun
   ((string-match "(defun.*" response)
    (let ((start (match-beginning 0)))
      (with-temp-buffer
        (insert response)
        (goto-char (1+ start))
        (condition-case nil
            (progn
              (forward-sexp)
              (buffer-substring start (point)))
          (error response)))))

   ;; Return as-is
   (t response)))

(defun test-iterative-try-eval (code)
  "Try to evaluate code and test it.
Returns (success-p result error)."
  (condition-case err
      (progn
        (eval (read code))
        ;; Try calling it
        (let ((result (eval '(fizzbuzz 15))))
          (list t result nil)))
    (error
     (list nil nil (error-message-string err)))))

;;;; Main Test Function

(defun test-iterative-save-attempt (attempt prompt response analysis eval-result)
  "Save a single attempt to results."
  (unless (file-directory-p test-iterative-results-dir)
    (make-directory test-iterative-results-dir t))

  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "%s/attempt-%d-%s.txt"
                          test-iterative-results-dir
                          attempt
                          timestamp)))
    (with-temp-file filename
      (insert (format "=== Attempt %d of %d ===\n\n"
                     attempt test-iterative-max-attempts))

      (insert "=== PROMPT ===\n")
      (insert prompt)
      (insert "\n\n")

      (insert "=== AI RESPONSE ===\n")
      (insert response)
      (insert "\n\n")

      (insert "=== ANALYSIS ===\n")
      (let* ((valid-p (nth 0 analysis))
             (mistakes (nth 1 analysis))
             (suggestions (nth 2 analysis)))
        (insert (format "Valid: %s\n\n" (if valid-p "✓ YES" "✗ NO")))

        (when mistakes
          (insert "Mistakes Found:\n")
          (dolist (mistake mistakes)
            (insert (format "  ✗ %s\n" mistake)))
          (insert "\n"))

        (when suggestions
          (insert "Suggestions for Next Attempt:\n")
          (dolist (suggestion suggestions)
            (insert (format "  → %s\n" suggestion)))
          (insert "\n")))

      (insert "=== EVALUATION ===\n")
      (let* ((success (nth 0 eval-result))
             (result (nth 1 eval-result))
             (error (nth 2 eval-result)))
        (if success
            (progn
              (insert "✓ Code evaluated successfully!\n")
              (insert (format "Result: %S\n" result)))
          (insert "✗ Code failed to evaluate\n")
          (when error
            (insert (format "Error: %s\n" error)))))

      (insert "\n=== END ===\n"))

    (message "Saved attempt %d to: %s" attempt filename)
    filename))

(defun test-iterative-fizzbuzz ()
  "Interactively test FizzBuzz generation with prompt improvement.

This will:
1. Try progressively better prompts
2. Analyze each response
3. Save all attempts to results/
4. Stop when valid code is generated

YOU MUST MANUALLY:
- Run gptel command with each prompt
- Copy the response
- Paste when prompted

This demonstrates the iterative improvement process."
  (interactive)

  ;; Check prerequisites
  (unless (featurep 'gptel)
    (error "gptel not loaded. Run: (require 'gptel)"))

  (unless (boundp 'gptel-backend)
    (error "gptel backend not configured. See ../elisp/setup.el"))

  ;; Reset
  (setq test-iterative-current-attempt 0)
  (setq test-iterative-all-attempts '())

  (message "\n=== Iterative Prompt Improvement Test ===\n")
  (message "Testing with %d progressive prompts\n" (length test-iterative-prompts))

  ;; Process each prompt
  (catch 'done
    (dolist (prompt-info test-iterative-prompts)
      (let* ((attempt (car prompt-info))
             (prompt (cdr prompt-info)))

        (setq test-iterative-current-attempt attempt)

        (message "\n--- Attempt %d/%d ---" attempt (length test-iterative-prompts))
        (message "\nPrompt to send:\n%s\n%s\n%s"
                 (make-string 60 ?-)
                 prompt
                 (make-string 60 ?-))

        ;; Ask user to run gptel manually
        (message "\nNow:")
        (message "1. Open gptel: M-x gptel")
        (message "2. Paste the above prompt")
        (message "3. Wait for response")
        (message "4. Come back here and press RETURN when ready")

        (read-string "Press RETURN when you have the AI response ready: ")

        ;; Get response from user
        (let ((response (read-string "Paste the AI response here: ")))

          (when (string-empty-p response)
            (message "No response provided. Stopping.")
            (throw 'done nil))

          ;; Analyze
          (let* ((analysis (test-iterative-analyze-response response))
                 (valid-p (nth 0 analysis))
                 (mistakes (nth 1 analysis))
                 (suggestions (nth 2 analysis)))

            (message "\n=== Analysis ===")
            (message "Valid: %s" (if valid-p "✓ YES" "✗ NO"))

            (when mistakes
              (message "\nMistakes:")
              (dolist (mistake mistakes)
                (message "  ✗ %s" mistake)))

            (when suggestions
              (message "\nSuggestions:")
              (dolist (suggestion suggestions)
                (message "  → %s" suggestion)))

            ;; Try to evaluate
            (let* ((code (test-iterative-extract-code response))
                   (eval-result (test-iterative-try-eval code)))

              ;; Save attempt
              (test-iterative-save-attempt attempt prompt response analysis eval-result)

              ;; Check if successful
              (when (and valid-p (nth 0 eval-result))
                (message "\n✓✓✓ SUCCESS! Valid code generated on attempt %d ✓✓✓" attempt)
                (message "\nGenerated code:\n%s" code)
                (message "\nAll attempts saved to: %s" test-iterative-results-dir)
                (throw 'done t))

              ;; If not valid, continue to next prompt
              (unless valid-p
                (message "\n→ Will try with improved prompt next...")
                (sit-for 2))))))))

  (message "\n=== Test Complete ===")
  (message "Review results in: %s" test-iterative-results-dir)
  (message "\nTo view: M-x dired RET %s RET" test-iterative-results-dir))

;;;; Automated Example (for reference)

(defun test-iterative-fizzbuzz-automated-example ()
  "Example of what fully automated version would look like.

This is DOCUMENTATION ONLY - shows the ideal automated flow."
  (format "
IDEAL AUTOMATED VERSION:

(defun test-iterative-automated ()
  (dolist (prompt-info test-iterative-prompts)
    (let* ((prompt (cdr prompt-info))
           (response (gptel-request prompt :sync t))  ; Would need async
           (analysis (test-iterative-analyze-response response))
           (valid-p (nth 0 analysis)))

      (test-iterative-save-attempt ... all data ...)

      (when valid-p
        (message \"Success on attempt %%d\" (car prompt-info))
        (return))

      (sit-for 5))))  ; Wait between API calls

CHALLENGES:
1. gptel-request needs callbacks (async)
2. Need to extract code from markdown
3. Need to safely eval potentially broken code
4. Need timeout handling
5. Need rate limiting

This manual version demonstrates the concept
while waiting for full async implementation.
"))

;;;; Quick Summary Function

(defun test-iterative-print-summary ()
  "Print summary of all attempts."
  (interactive)
  (if (null test-iterative-all-attempts)
      (message "No attempts recorded yet. Run: M-x test-iterative-fizzbuzz")

    (message "\n=== Attempt Summary ===\n")
    (dolist (attempt test-iterative-all-attempts)
      (message "Attempt %d: %s"
               (nth 0 attempt)
               (if (nth 3 attempt) "✓ SUCCESS" "✗ FAILED")))

    (message "\nResults saved to: %s" test-iterative-results-dir)))

(provide 'test-iterative-prompt-improvement)

;;; test-iterative-prompt-improvement.el ends here
