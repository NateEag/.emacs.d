;;; lsp-sh.el --- Shell support for lsp-mode

;;; Commentary:

;; Copyright (C) 2018 Mike Wilkerson <wilkystyle@gmail.com>

;; Author: Mike Wilkerson <wilkystyle@gmail.com>
;; Version: 1.0
;; Package-Version: 20180913.158
;; Package-Requires: ((lsp-mode "3.0"))
;; Keywords: sh, shell, bash
;; URL: https://github.com/wilkystyle/lsp-sh

;;; Code:

(require 'lsp-mode)

(lsp-define-stdio-client lsp-sh
                         "sh"
                         #'(lambda () default-directory)
                         '("bash-language-server" "start"))

(provide 'lsp-sh)
;;; lsp-sh.el ends here
