;;; setup.el --- Setup configuration for org-ai -*- lexical-binding: t; -*-

;; This file demonstrates how to install and configure org-ai using use-package.
;; Prerequisites:
;; - OpenAI API key (for ChatGPT, DALL-E, Whisper)
;; - Optionally: API keys for other providers if using custom backends

;;; Commentary:
;; org-ai integrates AI capabilities directly into org-mode, allowing you to
;; interact with LLMs using special org blocks and commands.

;;; Code:

(use-package org-ai
  :ensure t
  :after org

  ;; Keybindings - available in org-mode buffers
  :hook (org-mode . org-ai-mode)

  :bind (:map org-ai-mode-map
              ("C-c M-a" . org-ai-mode-map))

  :init
  ;; Set your OpenAI API key
  ;; Option 1: Set directly (not recommended for shared configs)
  ;; (setq org-ai-openai-api-token "your-api-key-here")

  ;; Option 2: Use auth-source (recommended)
  ;; Add to ~/.authinfo.gpg:
  ;; machine api.openai.com login org-ai password your-api-key-here

  ;; Option 3: Use environment variable
  (setq org-ai-openai-api-token (getenv "OPENAI_API_KEY"))

  :config
  ;; ===============================================
  ;; Model Configuration
  ;; ===============================================

  ;; Default ChatGPT model
  (setq org-ai-default-chat-model "gpt-4o")

  ;; Available models: gpt-4o, gpt-4-turbo, gpt-3.5-turbo, etc.

  ;; ===============================================
  ;; Behavior Settings
  ;; ===============================================

  ;; Auto-fill in org-ai blocks (wrap text)
  (setq org-ai-auto-fill t)

  ;; Default chat system prompt (customize for your use case)
  (setq org-ai-default-chat-system-prompt
        "You are a helpful assistant working within an Emacs org-mode document. Provide clear, concise answers.")

  ;; Use syntax highlighting in code blocks
  (setq org-ai-use-font-lock t)

  ;; Image directory for DALL-E generated images
  (setq org-ai-image-directory (expand-file-name "org-ai-images" org-directory))

  ;; ===============================================
  ;; Advanced Settings
  ;; ===============================================

  ;; Speech input/output (requires additional setup)
  ;; (setq org-ai-talk-spoken-input t)
  ;; (setq org-ai-talk-spoken-output t)

  ;; Customize context size (how much surrounding text to include)
  ;; (setq org-ai-context-lines 20)

  ;; Enable inline suggestions while typing
  ;; (setq org-ai-global-mode t)
  )

;; Optional: Enhanced org-mode support packages
(use-package org
  :ensure t
  :config
  ;; Enable automatic line breaks in org-mode
  (add-hook 'org-mode-hook #'auto-fill-mode)

  ;; Better code block editing
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t))

;; Example org-ai block usage:
;;
;; #+begin_ai
;; [ME]: What is the difference between let and let* in Emacs Lisp?
;; #+end_ai
;;
;; Then press C-c C-c to send the query.
;;
;; For image generation:
;; #+begin_ai :image
;; [ME]: A serene mountain landscape at sunset
;; #+end_ai
;;
;; For code refactoring:
;; #+begin_ai :refactor
;; [ME]: Improve this function:
;; (defun my-func (x) (if (> x 0) (+ x 1) (- x 1)))
;; #+end_ai

;; Quick commands (available in org-mode with org-ai-mode enabled):
;; - C-c M-a c : Start new chat
;; - C-c M-a i : Generate image
;; - C-c M-a r : Refactor region
;; - C-c M-a m : Modify region with prompt
;; - C-c M-a t : Talk (speech input)

(provide 'setup)
;;; setup.el ends here
