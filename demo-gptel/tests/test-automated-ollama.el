;;; test-automated-ollama.el --- Automated gptel + Ollama testing -*- lexical-binding: t; -*-

;; WARNING: This file includes AUTO-CORRECTION as a LEARNING TOOL.
;; The auto-correction is NOT from the AI - it's MY code fixing AI mistakes.
;; This is useful for learning common patterns but does NOT demonstrate AI improvement.
;;
;; For a proper test of AI code generation improvement, use:
;;   test-ai-iterative.el
;;
;; This file is kept for educational purposes to show common AI mistakes.

;;; Code:

(require 'gptel)

(defvar test-automated-results-dir
  (expand-file-name "results" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory for test results.")

;;;; Code extraction

(defun test-automated-extract-code (response)
  "Extract elisp code from RESPONSE."
  (cond
   ;; Markdown code block with language
   ((string-match "```\\(?:elisp\\|emacs-lisp\\|lisp\\)\n\\(\\(?:.\\|\n\\)*?\\)```" response)
    (match-string 1 response))
   ;; Generic code block
   ((string-match "```\n\\(\\(?:.\\|\n\\)*?\\)```" response)
    (match-string 1 response))
   ;; Look for defun
   ((string-match "(defun fizzbuzz\\(?:.\\|\n\\)*" response)
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

;;;; Code analysis

(defun test-automated-analyze-code (code)
  "Analyze CODE for common AI mistakes.
Returns (mistakes suggestions)."
  (let ((mistakes '())
        (suggestions '()))

    ;; Check for infix % operator (C/Python syntax)
    (when (string-match-p "[a-z] % [0-9]" code)
      (push "Uses infix % operator (C/Python syntax - not valid in Lisp)" mistakes)
      (push "Use prefix notation: (mod i N) not 'i % N'" suggestions))

    ;; Check for %N syntax (common AI error)
    (when (string-match-p "%[0-9]" code)
      (push "Uses %N syntax (not valid in Emacs Lisp)" mistakes)
      (push "Replace with (mod i N)" suggestions))

    ;; Check for format t
    (when (string-match-p "format t" code)
      (push "Uses 'format t' (Common Lisp)" mistakes)
      (push "Replace with (message ...)" suggestions))

    ;; Check for loop
    (when (string-match-p "(loop\\s-+for" code)
      (push "Uses 'loop for' (Common Lisp)" mistakes)
      (push "Replace with (dotimes ...) or (dolist ...)" suggestions))

    ;; Check for defparameter
    (when (string-match-p "defparameter" code)
      (push "Uses 'defparameter' (Common Lisp)" mistakes)
      (push "Replace with (defvar ...) or (defconst ...)" suggestions))

    (list mistakes suggestions)))

;;;; Code testing

(defun test-automated-try-eval (code)
  "Try to evaluate CODE and test it.
Returns (eval-success-p eval-error test-success-p test-result test-error)."
  (let ((eval-success-p nil)
        (eval-error nil)
        (test-success-p nil)
        (test-result nil)
        (test-error nil))

    ;; Try to evaluate the function definition
    (condition-case err
        (progn
          (eval (read code))
          (setq eval-success-p t))
      (error
       (setq eval-error (error-message-string err))))

    ;; If eval succeeded, try calling the function
    (when eval-success-p
      (condition-case err
          (progn
            (setq test-result
                  (with-output-to-string
                    (let ((standard-output (current-buffer)))
                      (fizzbuzz 15))))
            (setq test-success-p t))
        (error
         (setq test-error (error-message-string err)))))

    (list eval-success-p eval-error test-success-p test-result test-error)))

;;;; Code correction

(defun test-automated-correct-code (code)
  "Generate a corrected version of CODE.
Returns corrected code string."
  (let ((corrected code))

    ;; Fix infix % syntax: "i % N" → "(mod i N)"
    (setq corrected (replace-regexp-in-string
                     "\\([a-z]+\\) % \\([0-9]+\\)"
                     "(mod \\1 \\2)"
                     corrected))

    ;; Fix %N syntax in comparisons: "(< i %3)" → "(zerop (mod i 3))"
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

;;;; Result saving

(defun test-automated-save-result (prompt response)
  "Save PROMPT and RESPONSE with full analysis."
  (unless (file-directory-p test-automated-results-dir)
    (make-directory test-automated-results-dir t))

  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "%s/test-result-%s.txt"
                          test-automated-results-dir
                          timestamp))
         (code (test-automated-extract-code response))
         (analysis (test-automated-analyze-code code))
         (mistakes (nth 0 analysis))
         (suggestions (nth 1 analysis))
         (test-result (test-automated-try-eval code))
         (eval-ok (nth 0 test-result))
         (eval-err (nth 1 test-result))
         (test-ok (nth 2 test-result))
         (test-output (nth 3 test-result))
         (test-err (nth 4 test-result))
         (corrected-code (when mistakes (test-automated-correct-code code))))

    (with-temp-file filename
      (insert "=== AUTOMATED TEST RESULT ===\n\n")
      (insert (format "Timestamp: %s\n" (current-time-string)))
      (insert (format "Backend: %s\n" (oref gptel-backend name)))
      (insert (format "Model: %s\n\n" gptel-model))

      (insert "=== PROMPT SENT ===\n")
      (insert prompt)
      (insert "\n\n")

      (insert "=== AI RESPONSE (RAW) ===\n")
      (insert response)
      (insert "\n\n")

      (insert "=== EXTRACTED CODE ===\n")
      (insert code)
      (insert "\n\n")

      (insert "=== ANALYSIS ===\n")
      (if mistakes
          (progn
            (insert "Status: ✗ MISTAKES FOUND\n\n")
            (insert "Mistakes:\n")
            (dolist (mistake mistakes)
              (insert (format "  ✗ %s\n" mistake)))
            (insert "\nSuggestions:\n")
            (dolist (suggestion suggestions)
              (insert (format "  → %s\n" suggestion)))
            (insert "\n"))
        (insert "Status: ✓ No obvious mistakes detected\n\n"))

      (insert "=== EVALUATION TEST ===\n")
      (if eval-ok
          (insert "✓ Code evaluated successfully (function defined)\n\n")
        (insert (format "✗ Code failed to evaluate\nError: %s\n\n" eval-err)))

      (when eval-ok
        (insert "=== EXECUTION TEST ===\n")
        (if test-ok
            (progn
              (insert "✓ Function executed successfully\n\n")
              (insert "Output:\n")
              (insert test-output)
              (insert "\n"))
          (insert (format "✗ Function failed to execute\nError: %s\n\n" test-err))))

      (when corrected-code
        (insert "=== CORRECTED CODE (WARNING: NOT FROM AI!) ===\n")
        (insert "NOTE: This correction was done by regex pattern matching,\n")
        (insert "NOT by the AI. This is a learning tool to show common mistakes.\n")
        (insert "For real AI improvement testing, use test-ai-iterative.el\n\n")
        (insert corrected-code)
        (insert "\n\n")

        (insert "=== TESTING CORRECTED CODE ===\n")
        (let ((corrected-test (test-automated-try-eval corrected-code)))
          (if (nth 0 corrected-test)
              (progn
                (insert "✓ Corrected code evaluates successfully\n\n")
                (when (nth 2 corrected-test)
                  (insert "✓ Corrected function executes successfully\n\n")
                  (insert "Output:\n")
                  (insert (nth 3 corrected-test))
                  (insert "\n")))
            (insert (format "✗ Corrected code still has issues\nError: %s\n\n"
                           (nth 1 corrected-test))))))

      (insert "=== SUMMARY ===\n")
      (insert (format "AI Generated Code: %s\n"
                     (if eval-ok "Evaluated OK" "Failed to evaluate")))
      (insert (format "Function Execution: %s\n"
                     (if test-ok "Passed" "Failed")))
      (insert (format "Mistakes Found: %d\n" (length mistakes)))
      (insert (format "Auto-correction: %s\n"
                     (if corrected-code "Applied" "Not needed")))
      (insert "\n=== END ===\n"))

    (message "Result saved to: %s" filename)
    filename))

;;;; Main test function

(defun test-automated-ollama-single ()
  "Test FizzBuzz generation with Ollama - fully automated."
  (interactive)

  (unless (featurep 'gptel)
    (error "gptel not loaded"))

  (unless (boundp 'gptel-backend)
    (error "gptel-backend not configured"))

  (let ((prompt "Write ONLY an Emacs Lisp function called fizzbuzz that takes argument n and prints FizzBuzz sequence from 1 to n. Use dotimes and message. Return ONLY the function definition."))

    (message "Sending prompt to Ollama...")
    (message "Backend: %s, Model: %s" (oref gptel-backend name) gptel-model)

    (gptel-request prompt
      :callback
      (lambda (response info)
        (if (not (stringp response))
            (message "Error: No valid response received")
          (message "✓ Response received (%d chars)" (length response))
          (test-automated-save-result prompt response)
          (message "\nCheck results directory for full analysis!"))))))

(provide 'test-automated-ollama)
;;; test-automated-ollama.el ends here
