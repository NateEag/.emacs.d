;;; nix-drv-mode.el --- Major mode for viewing .drv files -*- lexical-binding: t -*-

;; Maintainer: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix, languages, tools, unix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A major mode for viewing Nix derivations (.drv files). See the Nix
;; manual for more information available at
;; https://nixos.org/nix/manual/.

;;; Code:

(require 'json-ts-mode)
(require 'nix)

;;;###autoload
(define-derived-mode nix-drv-mode json-ts-mode "Nix-Derivation"
  "Pretty print Nix’s .drv files."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((err-buf (generate-new-buffer "*nix-drv-mode*")))
    (make-process
     :name "nix-drv-mode"
     :buffer (current-buffer)
     :command (list nix-executable "derivation" "show" (buffer-file-name))
     :stderr err-buf
     :sentinel (lambda (proc event)
		 (when (string-match "finished" event)
		   (let ((buf (process-buffer proc)))
                     (when (and buf (buffer-live-p buf))
                       (with-current-buffer buf
			 (set-buffer-modified-p nil)
			 (read-only-mode)
			 )))
		   (when (and (buffer-live-p err-buf) (> (buffer-size err-buf) 0))
		     (with-current-buffer err-buf
		       (read-only-mode))
		     (display-buffer err-buf))
		   ))
     :filter (lambda (proc output)
               ;; use process buffer for output
               (let ((buf (process-buffer proc)))
		 (when (and buf (buffer-live-p buf))
		   (with-current-buffer buf
		     (let ((inhibit-read-only t))
                       (goto-char (point-max))
                       (insert output))))))))
  (add-hook 'change-major-mode-hook #'nix-drv-mode-dejsonify-buffer nil t))

(defun nix-drv-mode-dejsonify-buffer ()
  "Restore nix-drv-mode when switching to another mode."

  (remove-hook 'change-major-mode-hook #'nix-drv-mode-dejsonify-buffer t)

  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents (buffer-file-name))
    (set-buffer-modified-p nil)
    (read-only-mode -1)))

;;;###autoload
(add-to-list 'auto-mode-alist '("^/nix/store/.+\\.drv\\'" . nix-drv-mode))

(provide 'nix-drv-mode)
;;; nix-drv-mode.el ends here
