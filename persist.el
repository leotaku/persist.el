;;; persist.el --- Persistent variables without desktop-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 First Last

;; Author: First Last <name@example.com>
;; URL: http://example.com/package-name.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: something

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Variables:

(defvar persist/top-level-dir
  (cond ((featurep 'no-littering)
         no-littering-var-directory)
        (t
         (f-join user-emacs-directory "persist"))))

(defvar persist//loaded-packages
  nil)

;;;; Public API:

(defmacro persist/save-variables (package &rest variables)
  (when (-contains-p persist//loaded-packages package)
    (error "persist: package %s already loaded" package))
  (let* ((file (persist//initialize-namespace package))
         (save-func (concat "persist-" package "//save-all-variables")))
    `(progn (defun ,(intern save-func) ()
              (persist//merge-variables ,file ',variables))
            (add-hook 'kill-emacs-hook ',(intern save-func))
            (persist//load-variables-from-file ,file ',variables)
            (add-to-list 'persist//loaded-packages ,package))))

(put 'persist/save-variables 'lisp-indent-function 'defun)

;;;; Library Functions

(defun persist//initialize-namespace (name)
  (let* ((dir (f-join persist/top-level-dir name))
         (persist-file (f-join dir ".persist-dir"))
         (variables-file (f-join dir "variables"))
         (exists? (f-file-p dir))
         (owned? (f-file-p persist-file)))
    (if exists?
        (unless owned?
          (error "persist: directory for %s exists, but isn't owned by persist.el" name))
      (f-mkdir dir)
      (f-touch persist-file)
      (write-region "()" nil variables-file))
    variables-file))

(defun persist//read-file (file)
  (let ((text (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
    (read text)))

(defun persist//load-variables-from-file (file variables-list)
  (--each (persist//read-file file)
    (when (-contains-p variables-list (car it))
      (set (car it) (cadr it)))))

(defun persist//merge-variables (file variables-list)
  (let* ((variables-alist (--map (list it (symbol-value it))
                                 variables-list))
         (string (format "%S" variables-alist)))
    (write-region string nil
                  file)))


