;;; setup.el --- Setup configuration for aider.el -*- lexical-binding: t; -*-

;; This file demonstrates how to install and configure aider.el using use-package.
;; Prerequisites:
;; - Install Aider: pip install aider-chat
;; - Configure API keys for your preferred LLM provider (OpenAI, Anthropic, etc.)

;;; Commentary:
;; aider.el provides an Emacs interface to Aider, an AI pair programming tool.
;; This setup demonstrates basic installation and configuration.

;;; Code:

(use-package aider
  ;; Install from MELPA
  :ensure t

  ;; Keybindings
  :bind (("C-c a a" . aider)
         ("C-c a c" . aider-transient-menu))

  :config
  ;; Set the path to the aider executable if not in PATH
  ;; (setq aider-program "~/.local/bin/aider")

  ;; Set default arguments for aider
  ;; (setq aider-args '("--model" "gpt-4"))

  ;; Anthropic Claude example:
  ;; (setq aider-args '("--model" "claude-3-5-sonnet-20241022"))

  ;; For local models via Ollama:
  ;; (setq aider-args '("--model" "ollama/codellama"))

  ;; Set API keys (alternatively, use environment variables)
  ;; (setenv "OPENAI_API_KEY" "your-key-here")
  ;; (setenv "ANTHROPIC_API_KEY" "your-key-here")

  ;; Auto-commit changes made by aider
  ;; (setq aider-auto-commits t)
  )

;; Alternative: Install from GitHub if not yet on MELPA
;; (use-package aider
;;   :straight (:host github :repo "tninja/aider.el")
;;   :bind (("C-c a a" . aider)
;;          ("C-c a c" . aider-transient-menu)))

(provide 'setup)
;;; setup.el ends here
