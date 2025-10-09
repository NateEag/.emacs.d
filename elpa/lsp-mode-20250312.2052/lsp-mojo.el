<<<<<<<< HEAD:elpa/lsp-mode-20250312.2052/lsp-mojo.el
;;; lsp-mojo.el --- lsp-mode Mojo integration -*- lexical-binding: t; -*-
|||||||| parent of fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20230823.446/lsp-v.el
;;; lsp-v.el --- lsp-mode V integration -*- lexical-binding: t; -*-
========
;;; lsp-fennel.el --- lsp-mode for the fennel-ls -*- lexical-binding: t; -*-
>>>>>>>> fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20250312.2052/lsp-fennel.el

<<<<<<<< HEAD:elpa/lsp-mode-20250312.2052/lsp-mojo.el
;; Copyright (C) 2023 Adam Liter
|||||||| parent of fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20230823.446/lsp-v.el
;; Copyright (C) 2021 remimimimi
========
;; Copyright (C) 2024 Merrick Luo
>>>>>>>> fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20250312.2052/lsp-fennel.el

<<<<<<<< HEAD:elpa/lsp-mode-20250312.2052/lsp-mojo.el
;; Author: Adam Liter <io@adamliter.org>
;; Keywords: languages,tools
|||||||| parent of fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20230823.446/lsp-v.el
;; Author: remimimimi
;; Keywords: languages,tools
========
;; Author: Merrick Luo
;; Keywords: languages
>>>>>>>> fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20250312.2052/lsp-fennel.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

<<<<<<<< HEAD:elpa/lsp-mode-20250312.2052/lsp-mojo.el
;;  client for Mojo 🔥
|||||||| parent of fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20230823.446/lsp-v.el
;;  client for vls, the V language server
========
;; LSP client for fennel-ls - an language server for fennel.
>>>>>>>> fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20250312.2052/lsp-fennel.el

;;; Code:

(require 'lsp-mode)

<<<<<<<< HEAD:elpa/lsp-mode-20250312.2052/lsp-mojo.el
(defgroup lsp-mojo nil
  "LSP support for Mojo 🔥, using mojo-lsp-server."
|||||||| parent of fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20230823.446/lsp-v.el
(defgroup lsp-v nil
  "LSP support for V via vls."
========
(defgroup lsp-fennel nil
  "LSP support for the fennel-ls language server."
>>>>>>>> fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20250312.2052/lsp-fennel.el
  :group 'lsp-mode
<<<<<<<< HEAD:elpa/lsp-mode-20250312.2052/lsp-mojo.el
  :link '(url-link "https://github.com/modularml/mojo"))
|||||||| parent of fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20230823.446/lsp-v.el
  :link '(url-link "https://github.com/vlang/vls/tree/master"))
========
  :link '(url-link "https://git.sr.ht/~xerool/fennel-ls"))
>>>>>>>> fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20250312.2052/lsp-fennel.el

<<<<<<<< HEAD:elpa/lsp-mode-20250312.2052/lsp-mojo.el
(defcustom lsp-mojo-executable "mojo-lsp-server"
  "The Mojo 🔥 LSP executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-mojo
  :type 'string)
|||||||| parent of fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20230823.446/lsp-v.el
(defcustom lsp-v-vls-executable "vls"
  "The vls executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
  :group 'lsp-v
  :type 'string)
========
;; TODO: consider find in luarocks install location
(defun lsp-fennel--ls-command ()
  (executable-find "fennel-ls"))
>>>>>>>> fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20250312.2052/lsp-fennel.el

(lsp-register-client
 (make-lsp-client
<<<<<<<< HEAD:elpa/lsp-mode-20250312.2052/lsp-mojo.el
  :new-connection (lsp-stdio-connection (lambda () lsp-mojo-executable))
  :activation-fn (lsp-activate-on "mojo")
  :server-id 'mojo))
|||||||| parent of fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20230823.446/lsp-v.el
  :new-connection (lsp-stdio-connection (lambda () lsp-v-vls-executable))
  :activation-fn (lsp-activate-on "V")
  :server-id 'v-ls))
========
  :new-connection (lsp-stdio-connection #'lsp-fennel--ls-command)
  :activation-fn (lsp-activate-on "fennel")
  :priority -2
  :server-id 'fennel-ls))
>>>>>>>> fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20250312.2052/lsp-fennel.el

<<<<<<<< HEAD:elpa/lsp-mode-20250312.2052/lsp-mojo.el
(lsp-consistency-check lsp-mojo)
|||||||| parent of fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20230823.446/lsp-v.el
(lsp-consistency-check lsp-v)
========
(lsp-consistency-check lsp-fennel)
>>>>>>>> fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20250312.2052/lsp-fennel.el

<<<<<<<< HEAD:elpa/lsp-mode-20250312.2052/lsp-mojo.el
(provide 'lsp-mojo)
;;; lsp-mojo.el ends here
|||||||| parent of fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20230823.446/lsp-v.el
(provide 'lsp-v)
;;; lsp-v.el ends here
========
(provide 'lsp-fennel)
;;; lsp-fennel.el ends here
>>>>>>>> fb6babe65 (Delete / add a bunch of elpa packages):elpa/lsp-mode-20250312.2052/lsp-fennel.el
