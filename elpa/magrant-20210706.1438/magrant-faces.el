;;; magrant-faces.el --- Faces for magrant  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Jordan Besly

;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Font-lock faces for package `magrant'.


;;; Code:



;; REQUIRES

(require 'magrant-core)



;; FACES

(defgroup magrant-faces nil
  "Magrant faces."
  :group 'magrant
  :group 'faces)

(defface magrant-face-status-up
  '((t :foreground "Green"))
  "Face used when the status is up."
  :group 'magrant-faces)

(defface magrant-face-status-down
  '((t :foreground "Red"))
  "Face used when the status is down"
  :group 'magrant-faces)

(defface magrant-face-status-other
  '((t :foreground "Gold"))
  "Face used when the status is not up/down."
  :group 'magrant-faces)




(provide 'magrant-faces)

;;; magrant-faces.el ends here
