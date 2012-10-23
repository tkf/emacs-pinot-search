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

(defface pinot:search-item-title
  '((t :height 1.1 :inherit (variable-pitch bold)))
  "Face for matched document title."
  :group 'pinot)

(defface pinot:search-item-link
  '((t :inherit font-lock-comment-face))
  "Face for matched document link (path)."
  :group 'pinot)

(defvar pinot:source-directory
  (or (and load-file-name (file-name-directory load-file-name))
      default-directory)
  "Directory where pinot.el and pinot-search-wrapper.py locate.")

(defvar pinot:stderr-file nil
  "File to save STDERR output from pinot-search command.
If `nil', STDERR will be discarded.  When it is specified, this
file pops up opened when pinot-search failed.")

(defvar pinot:search-wrapper
  (and (executable-find "python")
       (concat pinot:source-directory "pinot-search-wrapper.py"))
  "Path to pinot-search-wrapper.py.")

(defvar pinot:normalize-path 'identity
  "A function to convert path.  Useful when you have symlinks.")

(defvar pinot:search-executable
  (or pinot:search-wrapper "pinot-search"))
(defvar pinot:search-engine-type "xapian")
(defvar pinot:search-engine-option (expand-file-name "~/.pinot/daemon"))

(defvar pinot:search-args
  (unless pinot:search-wrapper '("--toxml" "-"))
  "Argument to give `pinot:search-executable'.

For normal pinot-search::

  (setq pinot:search-args '(\"--toxml\" \"-\"))

Use pinot-search-wrapper.py::

  (setq pinot:search-executable pinot:search-wrapper)
  (setq pinot:search-args nil)

Use pinot-search-wrapper.py with D-bus::

  (setq pinot:search-executable pinot:search-wrapper)
  (setq pinot:search-args '(\"--dbus\"))

FIXME: simplify the setting.
")

(defun* pinot:search-command (query &key (buffer t))
  "Insert search result of QUERY in BUFFER (default: current buffer)."
  (let* ((stderr pinot:stderr-file)
         (args (append pinot:search-args
                       (list pinot:search-engine-type
                             pinot:search-engine-option
                             query)))
         (exit-status
          (apply #'call-process
                 pinot:search-executable nil (list buffer stderr) nil args)))
    ;; Treat error case:
    (unless (equal exit-status 0)
      (let ((cmd (apply #'concat pinot:search-executable " " args)))
        (when stderr
          (with-current-buffer (get-buffer-create "*pinot:stderr*")
            (erase-buffer)
            (insert-file-contents stderr)
            (display-buffer (current-buffer))))
        (error "%s failed with code %s" cmd exit-status)))))

(defun pinot:search-get-xml (query)
  (with-temp-buffer
    (erase-buffer)
    (pinot:search-command query)
    (libxml-parse-xml-region (point-min) (point-max))))

(defun pinot:xml-node-value (node name)
  (first (xml-node-children (first (xml-get-children node name)))))

(defun pinot:link-to-path (link)
  (if (string-prefix-p "file://" link)
      (funcall pinot:normalize-path (substring link 7))
    link))

(defun pinot:make-display (title description link)
  (concat
   (propertize title 'face 'pinot:search-item-title)
   " [" (propertize link 'face 'pinot:search-item-link) "] "
   ": " description))

(defun pinot:search-get-candidates (query)
  (loop with root = (pinot:search-get-xml query)
        with items = (xml-get-children (first (xml-node-children root)) 'item)
        for doc in items
        for description = (pinot:xml-node-value doc 'description)
        for link = (pinot:xml-node-value doc 'link)
        for title = (pinot:xml-node-value doc 'title)
        for path = (pinot:link-to-path link)
        for display = (pinot:make-display title description path)
        collect (cons display path)))

(defun pinot:return-t (candidate) t)

(defun pinot:search-make-source (query-sym action)
  `((name . "Pinot search")
    (candidates
     . (lambda () (pinot:search-get-candidates ,query-sym)))
    (requires-pattern . 2)     ; it seems xapian fails with one letter
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
