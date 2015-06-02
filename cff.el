;;; cff.el --- Search of the C/C++ file header by the source and vice versa

;; Copyright (C) 2015 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>
;; Created: 2015-06-02
;; Version: 1.0.0
;; Keywords: find-file
;; URL: https://github.com/fourier/eff
;; Compatibility: GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is a replacement for the ff-find-other-file.
;; Add the following to your .emacs file:
;; 
;; (require 'cff)
;; ;; defines shortcut for find source/header file for the current
;; ;; file
;; (add-hook 'c++-mode-hook
;;           '(lambda ()
;;              (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
;; (add-hook 'c-mode-hook
;;           '(lambda ()
;;              (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
;;
;;; Issues:
;;
;;; TODO:
;;  Add possibility to traverse file gier
;; 
;;; Change Log:
;; 
;; 2015-06-02 (1.0.0)
;;    Initial Release.
;;
;;; Code:

(defvar cff-header-regexps '(("\\.h$" . (lambda (base) (concat base ".h")))
                             ("\\.hpp$" . (lambda (base) (concat base ".hpp")))
                             ("\\.hxx$" . (lambda (base) (concat base ".hxx"))))
  "Regexps used to determine if the file is a C/C++ header file.  List of pairs: regexp of the file extension and a function to construct filename by the given base name.")

(defvar cff-source-regexps '(("\\.c$" . (lambda (base) (concat base ".c")))
                             ("\\.cc$" . (lambda (base) (concat base ".cc")))
                             ("\\.cxx$" . (lambda (base) (concat base ".cxx")))
                             ("\\.cpp$" . (lambda (base) (concat base ".cpp"))))
  "Regexps used to determine if the file is a C/C++ source file.  List of pairs: regexp of the file extension anda function to construct filename by the given base name.")


(defvar cff-interface-regexps '(("If\\.h$" . (lambda (base) (concat base "If.h")))
                                ("If\\.hpp$" (lambda (base) (concat base "If.hpp"))))
  "Regexps used to determine if the file is a C++ interface file.  List of pairs: regexp of the file extension anda function to construct filename by the given base name.")


(defvar cff-header-dirs '("inc" "include" "privinc" "private_include")
  "A list of short directory names to look headers in.")
(defvar cff-source-dirs '("src" "source")
  "A list of short directory names to look sources in." )
(defvar cff-interface-dirs '("if")
  "A list of short directory names to look interfaces in.")

(defun cff-root-path (fname)
  "Return the root for the given filename `FNAME'.
Example:
\(root-path \"/Users/username/.emacs.d/init.el\")
=> \"/\"
\(root-path \"C:/Users/myname/Downloads/somefile.exe\")
=> \"C:/\""
  (dotimes (x (length fname))
    (when (eql (elt fname x) ?/)
      (return (substring fname 0 (1+ x))))))

(defun cff-top-repo-directory ()
  "Find the top level directory if the current directory is in a git/svn repo.
Otherwise return the root directory"
  (interactive)
  (let* ((current-dir (expand-file-name (file-name-directory (buffer-file-name))))
         (cff-top-repo-directory-for-file current-dir))))

(defun cff-top-repo-directory-for-file (filename)
  "Find the top-level directory for a file it is in a git/svn repo.
Otherwise return the root directory"
  (let* ((root (or (locate-dominating-file filename ".git") 
                   (locate-dominating-file filename ".svn")
                   (cff-root-path filename))))
    (expand-file-name (file-name-as-directory root))))


(defun cff-get-current-file-path()
  "Directory containing current opened file"
  (file-name-directory (buffer-file-name)))

(defun cff-is-header (filename)
  "Determines if the file is a header and returns a pair (regex, function to construct file name from the regex) if so"
  (find-if (lambda (x) (string-match (car x) filename)) cff-header-regexps))

(defun cff-is-source (filename)
  "Determines if the file is a source and returs a pair (regex, function to construct file name from the regex) if so"
  (find-if (lambda (x) (string-match (car x) filename)) cff-source-regexps))

(defun cff-is-interface (filename)
  "Determines if the file is a C++ interface and returs a pair (regex, function to construct file name from the regex) if so"
  (find-if (lambda (x) (string-match (car x) filename)) cff-interface-regexps))

(defun cff-file-type (fname)
  "Returns the symbol determining the file type:
'INTERFACE for C++ interface files,
'HEADER for C/C++ header files,
'SOURCE for C/C++ source files,
'UNKNOWN if not match anything above"
  (cond ((cff-is-interface fname) 'interface)
        ((cff-is-header fname) 'header)
        ((cff-is-source fname) 'source)
        (t 'unknown)))

(defun cff-file-type-string (type)
  "Returs a string by the file type symbol TYPE"
  (cond ((equal type 'interface) "interface")
        ((equal type 'header) "header")
        ((equal type 'source) "source")
        (t  "unknown")))

(defun find-last-match (substr str)
  "Find the position of the last match of the substring SUBSTR in the string STR.
Return nil if not found."
  (let ((last-match nil)
        (match (string-match substr str 0)))
    (when match
      (setf last-match match)
      (while (setf match (string-match substr str (1+ last-match)))
              (setf last-match match)))
    last-match))

(defun replace-last-match (substr to str)
  "Replaces the last match SUBSTR in the string STR to the string TO.
Return new string or nil if failed"
  (let ((match (find-last-match substr str)))
    (when match
      (concat (substring str 0 match)
               (replace-regexp-in-string substr to str nil nil nil match)))))


(defun cff-do-find-other-file (top-dir current-dir subdirs predicate)
  (let ((found (cff-find-files-with top-dir
                                    fdir
                                    subdirs
                                    predicate))
    (cff-process-found found))))


(defun cff-process-found (found)
  (if found
      (if (> (length found) 1)
          ;; TODO present a menu
          (find-file (car found))
        ;; only one
        (find-file (car found)))
    (message "Not found")))
  

(defun cff-find-other-file ()
  "Find the appropriate header, source or interface file for the current file"
  (interactive)
  (let* ((fname (expand-file-name (buffer-file-name)))     ; full file name
         (ftype (cff-file-type fname))  ; file type
         (fdir (file-name-directory fname)) ; directory where the file is
         (fname-without-ext (file-name-base fname)) ; base file name (without extension)
         (top-dir (cff-top-repo-directory-for-file fname)))         ; repo top directory
    (cond ((eql ftype 'header)
           (cff-do-find-other-file top-dir
                                   fdir
                                   cff-source-dirs
                                   #'(lambda (x)
                                       (cff-is-source-for-header x fname))))
          ((eql ftype 'source)
           (cff-do-find-other-file top-dir
                                   fdir
                                   cff-header-dirs
                                   #'(lambda (x)
                                   (cff-is-header-for-source x fname))))
          ((eql ftype 'interface)
           (cff-do-find-other-file top-dir
                                   fdir
                                   cff-source-dirs
                                   #'(lambda (x)
                                   (cff-is-source-for-interface x fname))))
          (t (message "Not implemented")))))

;; algorithm:
;; guess if source, header or interface
;; example for header:
;; 1. search current directory for all source regexps (from cff-source-regexps)
;; if found, take the basename of the header, use the function to construct
;; an appropriate source file name and compare them to the found source file
;; if match, add to the list
;; 2. otherwise go up directory
;; find all 'src' (cff-source-dir) subdirectories
;; repeat step 1
;; repeat step 2 until current directory is not (cff-top-repo-directory)
;; TODO: if still not found, create a directory hierarchy replacing src with inc and vice-versa
;; and try to find it there


(defun cff-find-file-in-subdir (dir criteria)
  (let ((files (directory-files dir)))
        (find-if criteria files)))

(defun cff-find-files-with (top-dir dir subdirs criteria)
  "Returns the lisf of full paths to files which complies CRITERIA,
starting from the  DIR or its SUBDIRS and movig up to the TOP-DIR"
    (cff-find-files-with-iter top-dir dir subdirs criteria nil))
     
(defun cff-find-files-with-iter (top-dir dir subdirs criteria acc)
  ;; first try to look in the dir
  (let ((found (cff-find-file-in-subdir dir criteria)))
    ;; if push the result to the accumulator
    (when found (pushnew (concat dir found) acc)))
  ;; then look in all listed subdirs of the dir
  (let* ((fulldir (file-name-as-directory dir))
         (full-subdirs
          (remove-if-not 'file-exists-p
                         (mapcar #'(lambda (d) (concat fulldir (file-name-as-directory d)))
                                 subdirs))))
    (dolist (d full-subdirs)
      (let ((fname (cff-find-file-in-subdir d criteria)))
        (when fname
          (pushnew (concat d fname) acc)))))
  ;; now verify if we are in the top dir
  (if (string= top-dir dir)
      acc                               ; return accumulated paths
    ;; otherwise repeat with the parent directory
    (cff-find-files-with-iter top-dir (file-name-directory (directory-file-name dir))
                              subdirs criteria acc)))

(defun cff-find-other-file-source (top-dir filename)
  "Find the source file for the header FILENAME given, traversing directories
up to the TOP-DIR"
  (let ((short-name (file-name-base filename))
        (fdir (file-name-directory filename)))
    (cff-find-file-in-hierarchy top-dir
                                fdir
                                cff-header-dirs #'(lambda (x)
                                                    (string= (file-name-base x) short-name)))))


(defun cff-is-header-for-source (header source)
  "Determines if the HEADER (short file name) corresponds to the SOURCE (full file path)"
 (let ((basename (file-name-base source))
        ;; find if it is a header file
        (found (cff-is-header header)))
    (when (and found
               ;; ok it is a header
               ;; construct its name (from the second argument of map regexp to
               ;; function constructing the name) to the header name
               (string= header (funcall (cdr found) basename)))
               header)))

(defun cff-is-source-for-header (source header)
  "Determines if the SOURCE (short file name) corresponds to the HEADER (full file path)"
  (let ((basename (file-name-base header))
        ;; find if it is a source file
        (found (cff-is-source source)))
    (when (and found
               ;; ok it is a source
               ;; construct its name (from the second argument of map regexp to
               ;; function constructing the name) to the source name
               (string= source (funcall (cdr found) basename)))
               source)))

(defun cff-is-source-for-interface (source header)
  "Determines if the SOURCE (short file name) corresponds to the interface HEADER (full file path)"
  (cff-is-source-for-header source header))


(provide 'cff)
;;; cff.el ends here
