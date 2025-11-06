;;; setup.el --- Setup configuration for Ellama -*- lexical-binding: t; -*-

;; This file demonstrates how to install and configure Ellama using use-package.
;; Prerequisites:
;; - Install Ollama: https://ollama.ai/
;; - Pull at least one model: ollama pull llama2 (or codellama, mistral, etc.)

;;; Commentary:
;; Ellama provides an Emacs interface to LLMs via Ollama.
;; This setup demonstrates installation and common configuration options.

;;; Code:

;; Required dependency: llm package
(use-package llm
  :ensure t)

(use-package ellama
  :ensure t
  :after llm

  ;; Keybindings
  :bind (("C-c e c" . ellama-chat)
         ("C-c e a" . ellama-ask-about)
         ("C-c e l" . ellama-ask-line)
         ("C-c e s" . ellama-ask-selection)
         ("C-c e t" . ellama-translate)
         ("C-c e d" . ellama-define-word)
         ("C-c e i" . ellama-improve-wording)
         ("C-c e w" . ellama-improve-grammar)
         ("C-c e m" . ellama-make-list)
         ("C-c e f" . ellama-make-format)
         ("C-c e r" . ellama-render)
         ("C-c e e" . ellama-code-edit)
         ("C-c e a" . ellama-code-add)
         ("C-c e p" . ellama-code-improve)
         ("C-c e x" . ellama-complete))

  :init
  ;; Set up Ollama provider
  (require 'llm-ollama)

  ;; Configure the default provider and model
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "llama3.2:latest"
           :embedding-model "nomic-embed-text"))

  ;; Alternative models you can try:
  ;; - codellama:latest (good for coding)
  ;; - mistral:latest (fast and capable)
  ;; - llama2:latest (general purpose)
  ;; - deepseek-coder:latest (excellent for code)

  :config
  ;; Set the Ollama host if not using default localhost:11434
  ;; (setq ellama-ollama-host "http://localhost:11434")

  ;; Set naming strategy for sessions
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)

  ;; Set the directory for session files
  (setopt ellama-sessions-directory (expand-file-name "ellama-sessions" user-emacs-directory))

  ;; Enable markdown formatting in chat buffers
  (setopt ellama-chat-done-callback
          (lambda ()
            (when (derived-mode-p 'markdown-mode)
              (markdown-display-inline-images))))

  ;; Auto-save sessions
  (setopt ellama-auto-save t)

  ;; Set translation target language
  (setopt ellama-language "English")

  ;; Keymap prefix (alternative to individual bindings above)
  ;; (setq ellama-keymap-prefix "C-c e")
  )

;; Optional: Install markdown-mode for better chat formatting
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(provide 'setup)
;;; setup.el ends here
