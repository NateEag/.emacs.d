;;; nix-format.el --- Nix formatter -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; Homepage: https://github.com/NixOS/nix-mode

;;; Commentary:

;; Uses reformatter to define nix-format-buffer and
;; nix-format-on-save-mode.

;;; Code:

(require 'reformatter)

(defcustom nix-nixfmt-bin "nixfmt"
	"Path to nixfmt executable."
	:group 'nix
	:type 'string)

(defcustom nix-nixfmt-args '("-")
	"Command-line arguments for nixfmt."
	:group 'nix
	:type '(repeat string))

;;;###autoload (autoload 'nix-format-buffer "nix-format" nil t)
;;;###autoload (autoload 'nix-format-region "nix-format" nil t)
;;;###autoload (autoload 'nix-format-on-save-mode "nix-format" nil t)
(reformatter-define nix-format
	:program nix-nixfmt-bin
	:args nix-nixfmt-args
	:group 'nix
	:lighter " nixfmt")

(provide 'nix-format)
;;; nix-format.el ends here
