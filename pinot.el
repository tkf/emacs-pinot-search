;;; pinot.el --- Emacs interface to pinot-search

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; pinot.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; pinot.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with pinot.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)

(eval-when-compile (defvar anything-c-source-find-files)
                   (defvar helm-c-source-find-files))
(declare-function anything-other-buffer "anything")
(declare-function helm-other-buffer "helm")


(defgroup pinot nil
  "pinot-search Emacs interface"
  :group 'applications
  :prefix "pinot:")

(defface pinot:search-file-name
  '((t (:inherit font-lock-doc-face)))
  "Face for matched file."
  :group 'pinot)

(defvar pinot:search-executable "pinot-search")
(defvar pinot:search-engine-type "xapian")
(defvar pinot:search-engine-option (expand-file-name "~/.pinot/daemon"))
(defvar pinot:search-args '("--toxml" "-"))

(defun* pinot:search-command (query &key (buffer t) (stderr nil))
  (apply #'call-process
         pinot:search-executable nil (list buffer stderr) nil
         (append pinot:search-args
                 (list pinot:search-engine-type
                       pinot:search-engine-option
                       query))))

(defun pinot:search-get-xml (query)
  (with-temp-buffer
    (erase-buffer)
    (pinot:search-command query)
    (libxml-parse-xml-region (point-min) (point-max))))

(defun pinot:xml-node-value (node name)
  (first (xml-node-children (first (xml-get-children node name)))))

(defun pinot:search-get-candidates (query)
  (loop with root = (pinot:search-get-xml query)
        with items = (xml-get-children (first (xml-node-children root)) 'item)
        for doc in items
        for description = (pinot:xml-node-value doc 'description)
        for link = (pinot:xml-node-value doc 'link)
        collect (cons (concat
                       (propertize (file-name-nondirectory link)
                                   'face 'pinot:search-file-name)
                       ": " description)
                      (if (string-prefix-p "file://" link)
                          (substring link 7)
                        link))))

(defun pinot:return-t (candidate) t)

(defun pinot:search-make-source (query-sym action)
  `((name . "Pinot search")
    (candidates
     . (lambda () (pinot:search-get-candidates ,query-sym)))
    (requires-pattern . 1)
    ;; Pattern match is not required since pinot already did the job:
    (match . (pinot:return-t))
    ;; Needed to redo search when the pattern in minibuffer is changed:
    (volatile)
    ;; This does not work...  That's why I'm using `action':
    ;; (type . file)
    (action . ,action)
    (delayed)))



;; Anything

;;;###autoload
(defun anything-pinot-search ()
  (interactive)
  (require 'anything-config nil t) ; to load `anything-c-source-find-files'.
  (anything-other-buffer (pinot:search-make-source
                          'anything-pattern
                          (cdr (assoc 'action anything-c-source-find-files)))
                         "*anything pinot-search*"))


;; Helm

;;;###autoload
(defun helm-pinot-search ()
  (interactive)
  (require 'helm-files nil t)     ; to load `helm-c-source-find-files'
  (helm-other-buffer (pinot:search-make-source
                      'helm-pattern
                      (cdr (assoc 'action helm-c-source-find-files)))
                     "*helm pinot-search*"))

(provide 'pinot)

;;; pinot.el ends here
