;;; eping.el --- Ping websites to check internet connectivity -*- lexical-binding: t -*-

;; Copyright © 2020 Sean Hutchings <seanhut@yandex.com>

;; Author: Sean Hutchings <seanhut@yandex.com>
;; Maintainer: Sean Hutchings <seanhut@yandex.com>
;; Created: 2020-10-16
;; Keywords: comm, processes, terminals, unix
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1.1-git
;; Homepage: https://github.com/sean-hut/eping
;; License: BSD Zero Clause License (SPDX: 0BSD)

;; Permission to use, copy, modify, and/or distribute this software
;; for any purpose with or without fee is hereby granted.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Change Log: For all notable changes see CHANGELOG.md

;;; Commentary:

;; Ping websites to check internet connectivity.
;;
;; Repository: https://github.com/sean-hut/eping
;;
;; Documentation: https://sean-hut.github.io/eping/

;;; Code:

(require 'subr-x)

(defvar eping-domain-options
  '("wikipedia.org" "startpage.com" "gnu.org")
  "List of domains that Eping will present as options.")

(defvar eping-number-pings-options '("5" "1" "10" "15" "20")
  "List of how many times to ping the domain.
Eping will present this as list to select from for users.")

(defun eping (domain number-pings &optional speak)
  "Check internet connectivity with ping.

DOMAIN is the domain to ping.
NUMBER-PINGS is how many times to ping the domain.
With prefix arg SPEAK, the output is spoken by espeak."
  (interactive
   (list (completing-read "Domain to ping: " eping-domain-options nil t)
         (completing-read "Number of pings: " eping-number-pings-options nil t)
         current-prefix-arg))

  (let ((command (list "ping" "-c" number-pings domain)))
    (make-process :name "eping"
                  :command command
                  :sentinel (if speak
                                'eping--sentinel-espeak-output
                              'eping--sentinel-minibuffer-output))))

(defun eping--sentinel-minibuffer-output (process event)
  "Output the process name and event with minibuffer.
PROCESS is the process the sentinel is watching.
EVENT is the processes change event."
  (message "%s %s" process (string-trim-right event)))

(defun eping--sentinel-espeak-output (process event)
  "Output the process name and event with eSpeak.
PROCESS is the process the sentinel is watching.
EVENT is the processes change event."
  (let* ((espeak-text (format "%s %s" process event))
         (command (list "espeak"  espeak-text)))
    (make-process :name "eping-sentinel-espeak-output"
                  :command command)))

(provide 'eping)
;;; eping.el ends here
