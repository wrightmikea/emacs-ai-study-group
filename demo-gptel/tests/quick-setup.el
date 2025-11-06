;;; quick-setup.el --- Quick setup for running iterative tests -*- lexical-binding: t; -*-

;; Evaluate this file to set up gptel + Ollama for testing
;; Prerequisites: ollama serve running locally

;;; Code:

;; 1. Bootstrap package system
(require 'package)
(unless (assoc "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless package--initialized
  (package-initialize))

;; 2. Install gptel if not already installed
(unless (package-installed-p 'gptel)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'gptel))

;; 3. Load gptel
(require 'gptel)

;; 4. Configure Ollama backend
(gptel-make-ollama "Ollama"
  :host "localhost:11434"
  :stream t
  :models '(llama3.2:latest
            codellama:latest
            mistral:latest
            qwen2.5-coder:latest))

;; 5. Set Ollama as default backend
(setq-default gptel-backend (gptel-make-ollama "Ollama"
                              :host "localhost:11434"
                              :stream t
                              :models '(llama3.2:latest)))

;; 6. Set default model (change to match what you have installed)
(setq-default gptel-model "llama3.2:latest")

(message "✓ gptel configured for Ollama!")
(message "Default model: %s" gptel-model)
(message "Now you can run: (test-iterative-fizzbuzz)")

(provide 'quick-setup)
;;; quick-setup.el ends here
