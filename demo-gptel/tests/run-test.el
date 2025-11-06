;;; run-test.el --- Run automated test and show results -*- lexical-binding: t; -*-

;; This script runs the automated test and displays results

;;; Code:

;; Setup
(load-file "quick-setup.el")
(load-file "test-automated-ollama.el")

;; Run single test first (faster)
(message "\n" "=".repeat(60))
(message "Running SINGLE automated test...")
(message "=".repeat(60))

(test-automated-ollama-single)

;; Wait a bit, then show how to run full test
(run-with-timer
 5 nil
 (lambda ()
   (message "\n\nTo run FULL iterative test with 5 prompts, evaluate:")
   (message "(test-automated-ollama-fizzbuzz)")))

(provide 'run-test)
;;; run-test.el ends here
