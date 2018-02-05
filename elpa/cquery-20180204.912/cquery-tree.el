;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'cquery-common)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defface cquery-call-tree-root-face
  nil
  "."
  :group 'cquery)

(defface cquery-call-tree-node-normal-face
  nil
  "."
  :group 'cquery)

(defface cquery-call-tree-node-base-face
  '((t (:foreground "red")))
  "."
  :group 'cquery)

(defface cquery-call-tree-node-derived-face
  '((t (:foreground "orange")))
  "."
  :group 'cquery)

(defface cquery-call-tree-mouse-face
  '((t (:background "green")))
  "."
  :group 'cquery)

(defface cquery-call-tree-icon-face
  '((t (:foreground "grey")))
  "."
  :group 'cquery)

;; ---------------------------------------------------------------------
;;   Tree node
;; ---------------------------------------------------------------------

(cl-defstruct cquery-call-tree-node
  name
  usr
  location
  has-callers
  call-type
  ;; local vars:
  parent
  children
  expanded)

(defun read-cquery-call-tree-node (data &optional parent)
  "Construct a call tree node from hashmap DATA and give it the parent PARENT"
  (let* ((location (gethash "location" data))
         (filename (string-remove-prefix lsp--uri-file-prefix (gethash "uri" location))))
    (make-cquery-call-tree-node
     :name (gethash "name" data)
     :usr (gethash "usr" data)
     :location (cons filename (gethash "start" (gethash "range" location)))
     :has-callers (gethash "hasCallers" data)
     :call-type (gethash "callType" data)
     :parent parent
     :expanded nil
     :children nil)))

(defun cquery-call-tree-node--request-children (node)
  "."
  (let ((usr (cquery-call-tree-node-usr node)))
    (when (string-match-p "^[0-9]+$" usr) ;; usr is no-usr for constructors
      (--map (read-cquery-call-tree-node it node)
             (lsp--send-request
              (lsp--make-request "$cquery/callTreeExpand"
                                 `(:usr ,usr))))
      )))

(defun cquery-call-tree--request-init ()
  "."
  (interactive)
  (cquery--cquery-buffer-check)
  (lsp--send-request
   (lsp--make-request "$cquery/callTreeInitial"
                      `(
                        :textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
                        :position ,(lsp--cur-position)))))


;; ---------------------------------------------------------------------
;;   Visualization
;; ---------------------------------------------------------------------

(defvar-local cquery-call-tree--root-nodes nil
  ".")

(defvar-local cquery-call-tree--visible-root nil
  ".")

(defvar-local cquery-call-tree--origin-win nil
  "Window that the current call tree was opened from.")

(defun cquery-call-tree--refresh ()
  (let ((p (point)))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setf (cquery-call-tree-node-expanded cquery-call-tree--visible-root) t)
    (cquery-call-tree--insert-node cquery-call-tree--visible-root 0)
    (goto-char p)
    (setq buffer-read-only t)))

(defun cquery-call-tree--insert-node (node depth)
  (let* ((prefix (cquery-call-tree--make-prefix node depth))
         (name (cquery-call-tree--make-string node depth)))
    (insert (propertize (concat prefix name "\n")
                        'depth depth
                        'cquery-call-tree-node node))
    (when (cquery-call-tree-node-expanded node)
      (when (and (cquery-call-tree-node-has-callers node)
                 (null (cquery-call-tree-node-children node)))
        (setf (cquery-call-tree-node-children node)
              (cquery-call-tree-node--request-children node)))
      (--map (cquery-call-tree--insert-node it (+ depth 1))
             (cquery-call-tree-node-children node)))))

(defun cquery-call-tree--make-string (node depth)
  "Propertize the name of NODE with the correct properties"
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      (propertize (cquery-call-tree-node-name node)
                  'face (if (= depth 0)
                            'cquery-call-tree-root-face
                          (pcase (cquery-call-tree-node-call-type node)
                            ('0 'cquery-call-tree-node-normal-face)
                            ('1 'cquery-call-tree-node-base-face)
                            ('2 'cquery-call-tree-node-derived-face)))
                  'mouse-face 'cquery-call-tree-mouse-face))))

(defun cquery-call-tree--make-prefix (node depth)
  "."
  (let* ((padding (make-string (* 2 depth) ?\ ))
         (symbol (if (= depth 0)
                     (if (cquery-call-tree-node-parent node)
                         "< "
                       "  ")
                   (if (cquery-call-tree-node-has-callers node)
                       (if (cquery-call-tree-node-expanded node) "v " "> ")
                     "| "))))
    (concat padding (propertize symbol 'face 'cquery-call-tree-icon-face))))

(defun cquery-call-tree--open ()
  "."
  (let ((lsp-ws lsp--cur-workspace)
        (root-nodes (-map 'read-cquery-call-tree-node
                          (cquery-call-tree--request-init)))
        (orig-buf (current-buffer)))
    (with-current-buffer (get-buffer-create "*cquery-call-tree*")
      (cquery-call-tree-mode)
      (setq lsp--cur-workspace lsp-ws
            cquery-call-tree--root-nodes root-nodes
            cquery-call-tree--origin-win (get-buffer-window orig-buf)
            cquery-call-tree--visible-root (car cquery-call-tree--root-nodes))
      (when (null cquery-call-tree--root-nodes)
        (user-error "Couldn't open a call tree from point"))
      (cquery-call-tree--refresh)
      (setq header-line-format nil)
      (setq mode-line-format nil)
      (goto-char 1)))
  (let ((win (display-buffer-in-side-window (get-buffer "*cquery-call-tree*") '((side . right)))))
    (set-window-margins win 1)
    (select-window win)
    (set-window-start win 1)
    (set-window-dedicated-p win t)
    ))

(defun cquery-call-tree--node-at-point ()
  (get-text-property (point) 'cquery-call-tree-node))

(defun cquery-call-tree--depth-at-point ()
  (get-text-property (point) 'depth))

;; ---------------------------------------------------------------------
;;   Actions
;; ---------------------------------------------------------------------

(defun cquery-call-tree-toggle-expand ()
  "Toggle expansion of node at point"
  (interactive)
  (when-let* ((node (cquery-call-tree--node-at-point)))
    (setf (cquery-call-tree-node-expanded node)
          (or (not (cquery-call-tree-node-expanded node))
              (= 0 (cquery-call-tree--depth-at-point))))
    (cquery-call-tree--refresh)))

(defun cquery-call-tree-select-parent ()
  "."
  (interactive)
  (let ((depth (cquery-call-tree--depth-at-point)))
    (if (null depth)
        (forward-line -1)
      (if (> depth 0)
          (while (and (>= (cquery-call-tree--depth-at-point) depth)
                      (= 0 (forward-line -1))))
        (when-let* ((parent (cquery-call-tree-node-parent (cquery-call-tree--node-at-point))))
          (setq cquery-call-tree--visible-root parent)
          (cquery-call-tree--refresh))))))

(defun cquery-call-tree-set-root ()
  "Set root to current node"
  (interactive)
  (when-let* ((node (cquery-call-tree--node-at-point)))
    (when (cquery-call-tree-node-has-callers node)
      (setq cquery-call-tree--visible-root node)
      (setf (cquery-call-tree-node-expanded node) t)
      (cquery-call-tree--refresh))))

(defun cquery-call-tree-go ()
  "Go to the definition of the function at point"
  (interactive)
  (when-let* ((node (cquery-call-tree--node-at-point)))
    (select-window cquery-call-tree--origin-win)
    (find-file (car (cquery-call-tree-node-location node)))
    (goto-char (lsp--position-to-point (cdr (cquery-call-tree-node-location node))))
    (pulse-momentary-highlight-one-line (point) 'next-error)))

(defun cquery-call-tree-look ()
  "Look at the definition of function at point"
  (interactive)
  (when-let* ((node (cquery-call-tree--node-at-point)))
    (with-selected-window cquery-call-tree--origin-win
      (find-file (car (cquery-call-tree-node-location node)))
      (goto-char (lsp--position-to-point (cdr (cquery-call-tree-node-location node))))
      (recenter)
      (pulse-momentary-highlight-one-line (point) 'next-error))))

(defun cquery-call-tree-expand-or-set-root ()
  "If the node at point is unexpanded expand it, otherwise set it as root"
  (interactive)
  (when-let* ((node (cquery-call-tree--node-at-point)))
    (when (cquery-call-tree-node-has-callers node)
      (if (cquery-call-tree-node-expanded node)
          (cquery-call-tree-set-root)
        (cquery-call-tree-toggle-expand)))))

(defun cquery-call-tree-collapse-or-select-parent ()
  "If the node at point is expanded collapse it, otherwise select its parent"
  (interactive)
  (when-let* ((node (cquery-call-tree--node-at-point)))
    (if (and (> (cquery-call-tree--depth-at-point) 0)
             (cquery-call-tree-node-expanded node))
        (cquery-call-tree-toggle-expand)
      (cquery-call-tree-select-parent))))

;; ---------------------------------------------------------------------
;;   Mode
;; ---------------------------------------------------------------------

(defvar cquery-call-tree-mode-map nil
  "Keymap uses with ‘lsp-ui-peek-mode’.")

(let ((map (make-sparse-keymap)))
  (define-key map (kbd "<tab>") 'cquery-call-tree-toggle-expand)
  (define-key map [mouse-1] 'cquery-call-tree-toggle-expand )
  (define-key map (kbd "<return>") 'cquery-call-tree-go)
  (define-key map (kbd "C-<return>") 'cquery-call-tree-look)
  (define-key map (kbd "<left>") 'cquery-call-tree-collapse-or-select-parent)
  (define-key map (kbd "<right>") 'cquery-call-tree-expand-or-set-root)
  (setq cquery-call-tree-mode-map map))

(define-derived-mode cquery-call-tree-mode special-mode "cquery-call-tree"
  "Mode for the call tree buffer")

(defun cquery-call-tree ()
  "Open call tree for function at point"
  (interactive)
  (cquery--cquery-buffer-check)
  (cquery-call-tree--open))

(provide 'cquery-tree)
;;; cquery-tree.el ends here
