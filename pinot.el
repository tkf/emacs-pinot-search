;;; pinot.el --- Emacs interface to pinot-search

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Version: 0.1

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
                   (defvar helm-c-source-find-files)
                   (defvar helm-alive-p))
(declare-function anything "anything")
(declare-function anything-window "anything")
(declare-function helm "helm")
(declare-function helm-update "helm")


(defgroup pinot nil
  "pinot-search Emacs interface"
  :group 'applications
  :prefix "pinot:")

(defconst pinot:version "0.1")

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
  (concat pinot:source-directory "pinot-search-wrapper.py")
  "Path to pinot-search-wrapper.py.")

(defvar pinot:normalize-path 'identity
  "A function to convert path.  Useful when you have symlinks.")

(defvar pinot:search-executable "pinot-search")
(defvar pinot:search-engine-type "xapian")
(defvar pinot:search-engine-option (expand-file-name "~/.pinot/daemon"))

(make-obsolete-variable
 'pinot:search-args
 "Sorry, `pinot:search-args' is not supported anymore. \
Please use `pinot:search-method-alist'."
 "0.1")

(defcustom pinot:search-method-alist
  `((nil     . (,pinot:search-executable "--toxml" "-"))
    (wrapper . (,pinot:search-wrapper))
    (dbus    . (,pinot:search-wrapper "--dbus")))
  "Method name to \"pinot-search\" command map."
  :group 'pinot)

(defcustom pinot:search-method
  (cond
   ((not (executable-find "python")) nil)
   ((= (call-process "python" nil nil nil "-c" "import dbus") 0) 'dbus)
   (t 'wrapper))
  "Pinot search command to use.
Choose the one from the methods registered in
`pinot:search-method-alist'."
  :group 'pinot)

(defcustom pinot:default-input ""
  "Default initial input for helm/anything search."
  :group 'pinot)

(defcustom pinot:helm (if (featurep 'helm) 'helm 'anything)
  "Set this to `anything' to use `anything' instead of `helm'."
  :group 'pinot)

(defun pinot:helm-update ()
  (let ((update (intern (format "%s-update" pinot:helm)))
        (alive-p (if (eq pinot:helm 'helm)
                     helm-alive-p
                   (anything-window))))
    (when alive-p
      (funcall update))))

(defvar pinot:reply-candidates nil)
(defvar pinot:last-query nil)

(defun pinot:search-sentinel (process event)
  (cond
   ((equal event "finished\n")
    (setq pinot:reply-candidates
          (with-current-buffer (process-buffer process)
            (pinot:search-get-candidates
             (libxml-parse-xml-region (point-min) (point-max)))))
    (pinot:helm-update))
   (t
    (message "Error in pinot-search: %S" event))))

(defun pinot:search-command (query)
  "Call search command with QUERY."
  (if (equal pinot:last-query query)
      pinot:reply-candidates
    (setq pinot:last-query query)
    (setq pinot:reply-candidates nil)
    (let* ((stderr pinot:stderr-file)
           (method (assoc-default pinot:search-method
                                  pinot:search-method-alist))
           (program (car method))
           (args (append (cdr method)
                         (list pinot:search-engine-type
                               pinot:search-engine-option
                               query)))
           (buffer (get-buffer-create " *pinot:search*"))
           (process
            (progn
              (with-current-buffer buffer
                (erase-buffer))
              (apply #'start-process "pinot:search" buffer
                     program args))))
      (set-process-sentinel process #'pinot:search-sentinel)
      pinot:reply-candidates)))

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

(defun pinot:search-get-candidates (xml)
  (loop with root = xml
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
    (header-name . (lambda (x) (format "%s
Filters: site/file/ext/title/url/dir/inurl/lang/type/class/label"
                                      x)))
    (candidates
     . (lambda () (pinot:search-command ,query-sym)))
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
  (anything
   :sources (pinot:search-make-source
             'anything-pattern
             (cdr (assoc 'action anything-c-source-find-files)))
   :buffer "*anything pinot-search*"
   :input pinot:default-input))


;; Helm

;;;###autoload
(defun helm-pinot-search ()
  (interactive)
  (require 'helm-files nil t)     ; to load `helm-c-source-find-files'
  (helm
   :sources (pinot:search-make-source
             'helm-pattern
             (cdr (assoc 'action helm-c-source-find-files)))
   :buffer "*helm pinot-search*"
   :input pinot:default-input))

(provide 'pinot)

;;; pinot.el ends here
