;;; use-package-treesit.el --- Automatically use tree-sitter enhanced major modes -*- lexical-binding: t -*-

;; Copyright (C) 2023 Robert Enzmann
;; Copyright (C) 2026 École Polytechnique Fédérale de Lausanne (EPFL)

;; Author: Dominique Quatravaux <dominique@quatravaux.org>
;; Keywords: lisp tree-sitter treesit auto automatic use-package
;; URL: https://github.com/domq/use-package-treesit.git
;; Package-Version: 20260416.1048
;; Package-Revision: 398ca0df10f2
;; Package-Requires: ((emacs "30.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Provides the `(use-package foo :treesit)' idiom.
;;
;; Adding the `:treesit' keyword to your package's `use-package'
;; stanza, causes the corresponding treesit grammar to be downloaded
;; and installed automatically the first time the package loads.
;;
;; This is lazily implemented in terms of (or by adding advice to)
;; Emacs 30+'s built-in mechanisms. Specifically,
;;
;; - at the time the `use-package' stanza is evaluated, and provided
;;   the target package is available (or equivalently, in the same
;;   time frame and under the same preconditions as use-package's
;;   `:init' clause): an entry is added for the target language in
;;   `treesit-language-source-alist' unless one already exists;
;;
;; - before the target package actually loads:
;;   `treesit-install-language-grammar' is invoked (through advice on
;;   Emacs' standard functions for treesit support), and does the
;;   heavy lifting.
;;
;; The `:treesit' keyword to `use-package' accepts arguments as a
;; plist, with the same keys as in the `use-package-treesit-recipes' variable
;; i.e `:lang`, `:url`, `:source-dir`, `:cc` and `:c++` (all others,
;; including `:library` are ignored). These keywords and their values
;; set or overwrite the corresponding settings in variable
;; `use-package-treesit-recipes' for the target package.

;;; Code:

(require 'treesit)
(require 'use-package-core)
(eval-and-compile
  (require 'cl-lib)
  (require 'map)
  (require 'gv))

(defvar use-package-treesit-recipes
  '((:lang awk
     :library awk-ts-mode
     :url "https://github.com/Beaglefoot/tree-sitter-awk")
    (:lang bash
     :library bash-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-bash")
    (:lang bibtex
     :library bibtex-ts-mode
     :url "https://github.com/latex-lsp/tree-sitter-bibtex")
    (:lang blueprint
     :library blueprint-ts-mode
     :url "https://github.com/huanie/tree-sitter-blueprint")
    (:lang c
     :library c-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-c")
    (:lang c-sharp
     :library csharp-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (:lang clojure
     :library clojure-ts-mode
     :url "https://github.com/sogaiu/tree-sitter-clojure")
    (:lang cmake
     :library cmake-ts-mode
     :url "https://github.com/uyha/tree-sitter-cmake")
    (:lang commonlisp
     :library commonlisp-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
    (:lang cpp
     :library c++-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-cpp")
    (:lang css
     :library css-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-css")
    (:lang dart
     :library dart-ts-mode
     :url "https://github.com/ast-grep/tree-sitter-dart")
    (:lang dockerfile
     :library dockerfile-ts-mode
     :url "https://github.com/camdencheek/tree-sitter-dockerfile")
    (:lang elixir
     :library elixir-ts-mode
     :url "https://github.com/elixir-lang/tree-sitter-elixir")
    (:lang glsl
     :library glsl-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
    (:lang go
     :library go-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-go")
    (:lang gomod
     :library go-mod-ts-mode
     :url "https://github.com/camdencheek/tree-sitter-go-mod")
    (:lang heex
     :library heex-ts-mode
     :url "https://github.com/phoenixframework/tree-sitter-heex")
    (:lang html
     :library html-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-html")
    (:lang janet
     :library janet-ts-mode
     :url "https://github.com/sogaiu/tree-sitter-janet-simple")
    (:lang java
     :library java-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-java")
    (:lang javascript
     :library js-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-javascript"
     :revision "master"
     :source-dir "src")
    (:lang json
     :library json-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-json")
    (:lang julia
     :library julia-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-julia")
    (:lang kotlin
     :library kotlin-ts-mode
     :url "https://github.com/fwcd/tree-sitter-kotlin")
    (:lang latex
     :library latex-ts-mode
     :url "https://github.com/latex-lsp/tree-sitter-latex")
    (:lang lua
     :library lua-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-lua")
    (:lang magik
     :library magik-ts-mode
     :url "https://github.com/krn-robin/tree-sitter-magik")
    (:lang make
     :library makefile-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-make")
    (:lang markdown
     :library markdown-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
    (:lang nix
     :library nix-ts-mode
     :url "https://github.com/nix-community/tree-sitter-nix")
    (:lang nu
     :library nushell-ts-mode
     :url "https://github.com/nushell/tree-sitter-nu")
    (:lang org
     :library org-ts-mode
     :url "https://github.com/milisims/tree-sitter-org")
    (:lang perl
     :library perl-ts-mode
     :url "https://github.com/ganezdragon/tree-sitter-perl")
    (:lang prisma
     :library prisma-ts-mode
     :url "https://github.com/victorhqc/tree-sitter-prisma")
    (:lang proto
     :library protobuf-ts-mode
     :url "https://github.com/mitchellh/tree-sitter-proto")
    (:lang python
     :library python-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-python")
    (:lang r
     :library r-ts-mode
     :url "https://github.com/r-lib/tree-sitter-r")
    (:lang ruby
     :library ruby-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-ruby")
    (:lang rust
     :library rust-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-rust")
    (:lang scala
     :library scala-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-scala")
    (:lang sql
     :library sql-ts-mode
     :revision "gh-pages"
     :url "https://github.com/DerekStride/tree-sitter-sql")
    (:lang surface
     :library surface-ts-mode
     :url "https://github.com/connorlay/tree-sitter-surface")
    (:lang toml
     :library toml-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-toml")
    (:lang tsx
     :library tsx-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "master"
     :source-dir "tsx/src")
    (:lang typescript
     :library typescript-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "master"
     :source-dir "typescript/src")
    (:lang typst
     :library typst-ts-mode
     :url "https://github.com/uben0/tree-sitter-typst"
     :revision "master"
     :source-dir "src")
    (:lang verilog
     :library verilog-ts-mode
     :url "https://github.com/gmlarumbe/tree-sitter-verilog")
    (:lang vhdl
     :library vhdl-ts-mode
     :url "https://github.com/alemuller/tree-sitter-vhdl")
    (:lang vue
     :library vue-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-vue")
    (:lang wast
     :library wat-ts-wast-mode
     :url "https://github.com/wasm-lsp/tree-sitter-wasm"
     :source-dir "wast/src")
    (:lang wat
     :library wat-ts-mode
     :url "https://github.com/wasm-lsp/tree-sitter-wasm"
     :source-dir "wat/src")
    (:lang wgsl
     :library wgsl-ts-mode
     :url "https://github.com/mehmetoguzderin/tree-sitter-wgsl")
    (:lang yaml
     :library yaml-ts-mode
     :url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
  "All the treesit languages that `use-package-treesit' can install automatically.

You can add or alter elements in this list by passing a plist after
`:treesit` in the `use-package' stanza with the same keywords (except
`:library`), e.g.

  (use-package foo :treesit
                  (:lang foo-lang
                   :url \"https://github.com/foo/foo-treesit\"))

Each element of this list is a plist with the following keywords:

  :library
           The `features' symbol of the target package,
           i.e. the `foo` in `(use-package foo :treesit)`

  :lang
           The target package's language symbol, i.e. the key that
           should be created in variable
           `treesit-language-source-alist' if none exists. (In the
           opposite case, the existing entry is not modified and
           therefore none of the other keywords described below have
           any effect.)

           The value for `:lang` must match the symbol that the
           target package's Emacs Lisp code uses in its calls to
           `treesit-parser-create', `treesit-language-available-p',
           `treesit-ready-p' and so on.

  :url
           The URL to download the treesit grammar from, i.e. the
           `URL` (first) field in the alist value to be created in
           variable `treesit-language-source-alist'

  :revision (optional)
           The particular Git branch or tag to download the treesit
           grammar from, i.e. the `REVISION` (second) field in the
           alist value to be created in variable
           `treesit-language-source-alist'

  :source-dir (optional)
           The subdirectory in the source control to build the grammar
           from, i.e. the `SOURCE-DIR` (third) field in the alist
           value to be created in variable
           `treesit-language-source-alist'

  :cc (optional)
           The C compiler command to use, i.e. the `CC` (fourth) field
           in the alist value to be created in variable
           `treesit-language-source-alist'

  :c++ (optional)
           The C++ compiler command to use, i.e. the `C++` (fifth)
           field in the alist value to be created in variable
           `treesit-language-source-alist'")

(defun use-package-treesit--recipe-of-library (library)
  "Find a match for LIBRARY in the variable `use-package-treesit-recipes'."
  (let ((recipe (cl-find-if (lambda (it) (eq (plist-get it :library) library))
                            use-package-treesit-recipes)))
    (map-delete (copy-sequence recipe) :library)))

(defvar use-package-treesit-keyword :treesit)

(defun use-package-normalize/:treesit (name-symbol _keyword args)
  "`use-package' argument parser for the `:treesit' keyword.

NAME-SYMBOL is the name of the “regular” (Emacs Lisp) package being
configured (i.e. the first argument to the surrounding `use-package'
stanza.) ARGS is the list of arguments (sexps) situated between
`:treesit' and the next `use-package' keyword, or the end of the
`use-package' stanza. There should be no more than one such sexp,
which (if present) should be a plist with the same keywords as
an element of the variable `use-package-treesit-recipes'."
  (when (> (length args) 1)
    (error ":treesit arguments should be in a list"))
  (let* ((default-recipe (use-package-treesit--recipe-of-library name-symbol))
         (ret (map-merge 'plist default-recipe (car-safe args))))
    (cond (ret)
          (t (error "No default configuration known for treesit grammar %s"
                    name-symbol)))))

(defun use-package-handler/:treesit (name-symbol _keyword args rest state)
  "`use-package' handler for the `:treesit' keyword.

ARGS is the value returned previously by `use-package-normalize/:treesit'.
NAME-SYMBOL, REST and STATE are passed to `use-package-process-keywords`
as-is."
  (let ((body (use-package-process-keywords name-symbol rest state))
        (args-quoted
         (apply #'nconc (map-apply
                         (lambda (k v) (list k (if (symbolp v) `',v v)))
                         args))))
    (use-package-concat
     body
     `((use-package-treesit--prepare-auto-install ,@args-quoted)))))

(defun use-package-treesit--prepare-auto-install (&rest recipe)
  "Arrange for a treesit grammar to be lazily installed.

Add RECIPE into the variable `treesit-language-source-alist', where
`use-package-treesit--maybe-install-lazy' will pick it up."
  (setf (alist-get (plist-get recipe :lang) treesit-language-source-alist)
        (mapcar (lambda (c) (plist-get recipe c))
                '(:url :revision :source-dir :cc :c++))))

(defun use-package-treesit--maybe-install-lazy (language &rest _ignored)
  "If so configured, install LANGUAGE just before it will be required.

This function is used as before advice on core Emacs `treesit-ready-p'
and `treesit-parser-create' functions."
  (cond ((treesit-language-available-p language))
        ((assoc language treesit-language-source-alist)
         (progn
           (message "Installing the treesit grammar for %s" language)
           (treesit-install-language-grammar language)))))

(defun use-package-treesit--configure ()
  "Configure `use-package' for the `:treesit' keyword.

Insert the `:treesit' keyword (or whatever the value of the variable
`use-package-treesit-keyword' is) into the variable
`use-package-keywords' at the proper place, unless it is already there.
Place before-advice on `treesit-ready-p' and `treesit-parser-create'
functions, so as to install the grammars before running them."
  (unless (member use-package-treesit-keyword use-package-keywords)
    (setq use-package-keywords
          (let* ((before-pos (cl-position :custom use-package-keywords))
                 (head (cl-subseq use-package-keywords 0 before-pos))
                 (tail (nthcdr before-pos use-package-keywords)))
            (append head `(,use-package-treesit-keyword) tail))))
  (advice-add 'treesit-ready-p :before
              #'use-package-treesit--maybe-install-lazy)
  (advice-add 'treesit-parser-create :before
              #'use-package-treesit--maybe-install-lazy))

(use-package-treesit--configure)

(provide 'use-package-treesit)
;;; use-package-treesit.el ends here
