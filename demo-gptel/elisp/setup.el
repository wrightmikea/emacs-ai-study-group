;;; setup.el --- Setup configuration for gptel -*- lexical-binding: t; -*-

;; This file demonstrates how to install and configure gptel using use-package.
;; Prerequisites:
;; - API keys for your preferred LLM provider(s)
;; - For local models: Install Ollama, llama.cpp, or other compatible server

;;; Commentary:
;; gptel is a simple, no-frills LLM client for Emacs that supports multiple providers.
;; This setup demonstrates installation and configuration for various backends.

;;; Code:

(use-package gptel
  :ensure t

  ;; Keybindings
  :bind (("C-c g" . gptel-send)
         ("C-c G" . gptel-menu))

  :config
  ;; ===============================================
  ;; OpenAI Configuration (ChatGPT)
  ;; ===============================================
  ;; Set your API key (alternatively use environment variable OPENAI_API_KEY)
  ;; (setq gptel-api-key "your-openai-api-key-here")

  ;; Set default model
  (setq gptel-model "gpt-4o")

  ;; Available models: gpt-4o, gpt-4-turbo, gpt-3.5-turbo, etc.

  ;; ===============================================
  ;; Anthropic Claude Configuration
  ;; ===============================================
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda () (getenv "ANTHROPIC_API_KEY")) ;; or set directly
    :models '(claude-3-5-sonnet-20241022
              claude-3-5-haiku-20241022
              claude-3-opus-20240229))

  ;; Set Claude as default backend
  ;; (setq-default gptel-backend (gptel-make-anthropic "Claude"
  ;;                               :stream t
  ;;                               :key "your-anthropic-api-key"))
  ;; (setq-default gptel-model "claude-3-5-sonnet-20241022")

  ;; ===============================================
  ;; Google Gemini Configuration
  ;; ===============================================
  ;; (gptel-make-gemini "Gemini"
  ;;   :key (lambda () (getenv "GEMINI_API_KEY"))
  ;;   :stream t
  ;;   :models '(gemini-1.5-pro
  ;;             gemini-1.5-flash))

  ;; ===============================================
  ;; Ollama (Local Models) Configuration
  ;; ===============================================
  ;; (gptel-make-ollama "Ollama"
  ;;   :host "localhost:11434"
  ;;   :stream t
  ;;   :models '(llama3.2:latest
  ;;             codellama:latest
  ;;             mistral:latest
  ;;             deepseek-coder:latest))

  ;; ===============================================
  ;; llama.cpp Configuration
  ;; ===============================================
  ;; (gptel-make-gpt4all "llama-cpp"
  ;;   :host "localhost:8080"
  ;;   :models '(model-name))

  ;; ===============================================
  ;; General gptel Settings
  ;; ===============================================

  ;; Default temperature (0.0 to 2.0, higher = more creative)
  (setq gptel-temperature 0.7)

  ;; Max tokens in response
  (setq gptel-max-tokens 2000)

  ;; Show token count in mode line
  (setq gptel-use-header-line t)

  ;; Default prompt prefix
  (setq gptel-prompt-prefix-alist
        '((markdown-mode . "### ")
          (org-mode . "* ")
          (text-mode . "### ")))

  ;; Default response prefix
  (setq gptel-response-prefix-alist
        '((markdown-mode . "")
          (org-mode . "")
          (text-mode . "")))

  ;; Set default buffer mode for new gptel buffers
  (setq gptel-default-mode 'markdown-mode)

  ;; Track context for better conversations
  (setq gptel-track-response t)

  ;; Example: Set up multiple backends and switch between them
  ;; Use M-x gptel-menu (or C-c G) to switch backends interactively
  )

;; Optional: Install markdown-mode for better formatting
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; Example usage:
;; 1. M-x gptel - Start a new chat session
;; 2. Type your message
;; 3. C-c RET - Send message (or use keybinding C-c g)
;; 4. C-c G - Open menu to switch models/backends

(provide 'setup)
;;; setup.el ends here
