;;; setup.el --- Setup configuration for elisp-dev-mcp -*- lexical-binding: t; -*-

;; This file demonstrates how to configure Emacs to work with elisp-dev-mcp.
;; Prerequisites:
;; - Install Node.js and npm
;; - Install the elisp-dev-mcp server
;; - Configure your MCP client (e.g., Claude Desktop)

;;; Commentary:
;; elisp-dev-mcp is a Model Context Protocol (MCP) server for Emacs Lisp development.
;; It allows AI assistants to interact with your Emacs environment.
;;
;; Note: This primarily requires configuration on the MCP client side (e.g., Claude Desktop),
;; but this file shows Emacs-side utilities that may be helpful.

;;; Code:

;; Enable server mode to allow external connections
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; Enhanced elisp development support
(use-package elisp-mode
  :config
  ;; Show function documentation in echo area
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

  ;; Enable paredit for structured editing (optional)
  ;; (add-hook 'emacs-lisp-mode-hook #'paredit-mode)

  ;; Enable rainbow delimiters for better readability (optional)
  ;; (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  )

;; Example MCP client configuration (for Claude Desktop)
;; Add this to your Claude Desktop config file:
;;
;; macOS: ~/Library/Application Support/Claude/claude_desktop_config.json
;; Windows: %APPDATA%\Claude\claude_desktop_config.json
;;
;; {
;;   "mcpServers": {
;;     "elisp-dev": {
;;       "command": "npx",
;;       "args": ["-y", "elisp-dev-mcp"]
;;     }
;;   }
;; }

;; Optional: Install helper packages for elisp development
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(use-package elisp-demos
  :ensure t
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(provide 'setup)
;;; setup.el ends here
