;; -*- lexical-binding: t; -*-
;;; package --- ellama config
;;; Commentary:
;;; Agentic coding assistant via ellama + Ollama.
;;; Start Ollama locally: `ollama serve`
;;; Pull a model:  `ollama pull qwen2.5-coder:14b`
;;; Code:

(use-package ellama
  :straight (ellama :type git :host github :repo "s-kostyaev/ellama")
  :bind-keymap ("A-s SPC" . ellama-prefix-map)
  :init
  (setopt ellama-language "English"
          ellama-auto-scroll t
          ;; Keep context across chat turns
          ellama-sessions-directory (expand-file-name "ellama-sessions" user-emacs-directory))
  :config
  (require 'llm-ollama)
  ;; Primary model: a capable code-focused model.
  ;; Change the model name to whatever you have pulled locally.
  ;; Recommended: qwen2.5-coder:14b  or  deepseek-coder-v2:16b
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "qwen2.5-coder:14b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  ;; Summarisation / lighter tasks can use a smaller model
  (setopt ellama-summarization-provider
          (make-llm-ollama
           :chat-model "qwen2.5-coder:7b"
           :embedding-model "nomic-embed-text"))
  ;; Named model presets accessible via `ellama-provider-switch'
  (setopt ellama-providers
          '(("qwen2.5-coder:14b"    . (make-llm-ollama :chat-model "qwen2.5-coder:14b"    :embedding-model "nomic-embed-text"))
            ("qwen2.5-coder:7b"     . (make-llm-ollama :chat-model "qwen2.5-coder:7b"     :embedding-model "nomic-embed-text"))
            ("deepseek-coder-v2:16b" . (make-llm-ollama :chat-model "deepseek-coder-v2:16b" :embedding-model "nomic-embed-text")))))

(provide 'llm)
;;; llm.el ends here
