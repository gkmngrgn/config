;;; gtranslate.el --- Use google translate api to perform translations

;; Copyright (C) 2009 Bruno Tavernier

;; Author: Bruno Tavernier <tavernier.bruno@gmail.com>
;; Version: 0.1
;; Keywords: words, translation, language, googleapi

;; This file is NOT part of Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Inspired by text-translator.el
;; This utility allows for translation via the google translation api.
;;
;; Note: Feel free to replace `gtranslate-region-or-input' in the example
;; below by any function of your choice that return a string.
;;
;; As of now it work with GNU Emacs 23.x but not with XEmacs.

;; Installation
;; ============
;;
;; To install `gtranslate', make sure this file is saved in a directory in
;; your `load-path', and add the line:
;;
;;   (require 'gtranslate)
;; 
;; to your .emacs file.
;;
;; Or use the autoload mechanism instead
;;
;;   (autoload 'gtranslate-translate "gtranslate" nil t)
;;   (autoload 'gtranslate-translate-auto "gtranslate" nil t)
;; 
;; Then create a function for a one way translation, ex: English -> French
;;
;;   (defun my-en-fr ()
;;     (interactive)
;;     (gtranslate-translate (gtranslate-region-or-input) "en" "fr"))
;;
;; Alternatively you can define a function that work automatically
;; in two directions.
;; Note that it works for a Roman alphabet / Non-Roman alphabet
;; pair of language, ex: French <-> Japanese.
;;
;;   (defun my-fr-ja ()
;;     (interactive)
;;     (gtranslate-translate-auto (gtranslate-region-or-input) "fr" "ja"))
;;
;; Finally you can assign shortcut to the your functions
;;
;;   (global-set-key "\M-1" 'my-fr-ja)
;;   (global-set-key "\M-2" 'my-en-fr)

;;; Change Log:
;;
;; Version 0.1
;; * initial release


;;; Code:

;;; =====================================================================
;;;              Global variables and customization options

; Constants
(defconst gtranslate-version "0.1"
  "version number of this version of gtranslate.")

; Variables
(defcustom gtranslate-buffer "*translated*"
  "Buffer name that displays translation result.")

(defvar gtranslate-service "ajax.googleapis.com"
  "Service to use for translation.")

(defvar gtranslate-service-port 80
  "Port number of the service used for translation.")

(defvar gtranslate-user-agent "Emacs"
  "User agent displayed.")

;;; =====================================================================
;;;              Core functions

(defun gtranslate-make-url (text fl tl)
  "Generate the url to send to the translation service."
  (concat
   "v=1.0"
   (format "&q=%s" (url-hexify-string (encode-coding-string text 'utf-8)))
   (format "&langpair=%s" (url-hexify-string (encode-coding-string (format "%s|%s" fl tl) 'utf-8)))
   ))

(defun gtranslate-filter (proc str)
  "Remove the cruft from the service answer."
  (string-match "translatedText\":\"\\(\.*\\)\"\}" str)
  (with-current-buffer (process-buffer proc)
    (insert (match-string 1 str))))

(defun gtranslate-translate (text fl tl)
  "Translate 'text' from language 'fl' to language 'tl'"
  (get-buffer-create gtranslate-buffer)
  (let ((proc (open-network-stream "translation" gtranslate-buffer gtranslate-service gtranslate-service-port))
	(str (gtranslate-make-url text fl tl))
	(window (get-buffer-window gtranslate-buffer))
	(original-split-width-threshold split-width-threshold))
    (set-process-filter proc 'gtranslate-filter)
    (process-send-string proc (concat
			       "GET /ajax/services/language/translate?" str " HTTP/1.1\r\n"
			       "Accept-Encoding: identity\r\n"
			       "Host: " gtranslate-service "\r\n"
			       "Connection: Keep-Alive\r\n"
			       "Keep-Alive: 300\r\n"
			       "User-Agent: " gtranslate-user-agent "\r\n" "\r\n"
			       ))
    (message "Translating...")
    (save-selected-window
      (setq split-width-threshold nil) ; Split window horizontally
      (pop-to-buffer gtranslate-buffer)
      (setq split-width-threshold original-split-width-threshold) ; Restore setting
      (erase-buffer)
      (shrink-window-if-larger-than-buffer window)) ; Adjust window size
    ))

(defun gtranslate-translate-auto (text roman nonroman)
  "Choose automatically which translation to perform between one Roman alphabet and a non-roman alphabet language.
   Alphabet ration is 40%."
  (if (> (/ (* (length (replace-regexp-in-string "[^A-Za-z]+" "" text)) 100) (length text)) 40)
      (gtranslate-translate text roman nonroman)
      (gtranslate-translate text nonroman roman)))

;;; =====================================================================
;;;              Return string function

(defun gtranslate-region-or-input ()
  "Select region if active or ask user input for translation."
  (if (not mark-active)
      (setq text (read-string "String to translate: "))
    (setq text (buffer-substring (region-beginning) (region-end)))))

(provide 'gtranslate)

;;; gtranslate.el ends here.
