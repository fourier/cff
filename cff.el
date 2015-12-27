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
;; This is a replacement for the ff-find-other-file.  If the helm is loaded,
;; uses it to provide possible multiple choices; otherwise just picks the
;; first one.
;;
;; Usage:
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
;;  More options to find files (possibly in the whole git repository?)
;;
;;; Change Log:
;;
;; 2015-06-02 (1.0.0)
;;    Initial Release.
;;
;;; Code:

(require 'cl-lib)
;; optional helm dependency
(require 'helm nil t)

(defvar cff-no-choice nil
  "Determines what to do if there are several possible choices.
If t, always to use the first one.")

(defvar cff-header-regexps '(("\\.h$" . (lambda (base) (concat base ".h")))
                             ("\\.hpp$" . (lambda (base) (concat base ".hpp")))
                             ("\\.hxx$" . (lambda (base) (concat base ".hxx"))))
  "Regexps used to determine if the file is a C/C++ header file.  List of pairs:
regexp of the file extension and a function to construct filename by the given
base name.")

(defvar cff-source-regexps '(("\\.c$" . (lambda (base) (concat base ".c")))
                             ("\\.cc$" . (lambda (base) (concat base ".cc")))
                             ("\\.cxx$" . (lambda (base) (concat base ".cxx")))
                             ("\\.cpp$" . (lambda (base) (concat base ".cpp"))))
  "Regexps used to determine if the file is a C/C++ source file.  List of pairs:
regexp of the file extension anda function to construct filename by the given
base name.")


(defvar cff-interface-regexps '(("If\\.h$" . (lambda (base)
                                               (concat base "If.h")))
                                ("If\\.hpp$" (lambda (base)
                                               (concat base "If.hpp"))))
  "Regexps used to determine if the file is a C++ interface file.
List of pairs: regexp of the file extension anda function to construct filename
 by the given base name.")


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
      (cl-return (substring fname 0 (1+ x))))))


(defun cff-top-repo-directory-for-file (filename)
  "Find the top level directory for a file it is in a git/svn repo.
Otherwise return the root directory
Argument FILENAME the file name to find the topmost directory for."
  (cl-labels ((expand (fname) (expand-file-name (file-name-as-directory fname)))
              (get-vc-root (fname vcdir vcsymbol)
                           (let ((found (locate-dominating-file fname vcdir)))
                             (when found (cons (expand found) vcsymbol)))))
    (let* ((root (or (get-vc-root filename ".git" 'git)
                     (get-vc-root filename ".svn" 'svn)
                     (cons (expand (cff-root-path filename)) 'dir))))
      root)))



(defun cff-is-header (filename)
  "Determines if the file is a header and return a pair:
\(regex, function) to
construct file name from the regex) if so.
Argument FILENAME the potential header file name."
  (cl-find-if (lambda (x) (string-match (car x) filename)) cff-header-regexps))

(defun cff-is-source (filename)
  "Determines if the file is a source and return a pair:
\(regex, function) to
construct file name from the regex) if so.
Argument FILENAME potetntial source file name."
  (cl-find-if (lambda (x) (string-match (car x) filename)) cff-source-regexps))

(defun cff-is-interface (filename)
  "Determines if the file is a C++ interface and return a pair:
\(regex, function)
to construct file name from the regex) if so.
Argument FILENAME potential interface(special convention for header) file name."
  (cl-find-if (lambda (x) (string-match (car x) filename)) cff-interface-regexps))

(defun cff-file-type (fname)
  "Return the symbol determining the file type:
'INTERFACE for C++ interface files,
'HEADER for C/C++ header files,
'SOURCE for C/C++ source files,
'UNKNOWN if not match anything above
Argument FNAME file name to determine a type."
  (cond ((cff-is-interface fname) 'interface)
        ((cff-is-header fname) 'header)
        ((cff-is-source fname) 'source)
        (t 'unknown)))

(defun cff-find-last-match (substr str)
  "Find the position of the last match of the substring SUBSTR.
STR is the given string.
Return nil if not found."
  (let ((last-match nil)
        (match (string-match substr str 0)))
    (when match
      (setf last-match match)
      (while (setf match (string-match substr str (1+ last-match)))
        (setf last-match match)))
    last-match))

(defun cff-replace-last-match (substr to str)
  "Replace the last match SUBSTR to the string TO in the string STR.
Return new string or nil if failed"
  (let ((match (cff-find-last-match substr str)))
    (when match
      (concat (substring str 0 match)
              (replace-regexp-in-string substr to str nil nil nil match)))))


(defun cff-find-replacement (filename filetype)
  "Return the potential directory name for given FILENAME and FILETYPE.
Determines if the file path contains one of the directories from defined lists
to construct possible path to another file.  Returns this directory short name
\(i.e.  'source', 'inc' etc)."
  (let ((repl-list
         (cond ((eql filetype 'header)
                cff-header-dirs)
               ((eql filetype 'interface)
                cff-interface-dirs)
               ((eql filetype 'source)
                cff-source-dirs)
               (t nil))))
    (when repl-list
      (dolist (d repl-list)
        (when (cff-find-last-match (concat "/" d "/") filename)
          (cl-return d))))))


(defun cff-find-files-with-path (file replace-dir subdirs regexps)
  "Find all possible FILEs by replacing dirs like inc -> src.
Argument REPLACE-DIR replacement.
Argument SUBDIRS list of subdirectories to look for FILE.
Argument REGEXPS list of regexps to find."
  (when replace-dir ; files in path like (src|inc)/somepath/to/myfile.cpp
    (let ((basename (file-name-base file))
          (basedir (file-name-as-directory (file-name-directory file)))
          (replace-fragment (concat "/" replace-dir "/"))
          (results nil))
      (dolist (d subdirs)
        ;; construct possible directory where we could find our source/header
        (let ((possible-dir (cff-replace-last-match replace-fragment
                                                    (concat "/" d "/") basedir)))
          ;; if able to construct directory
          (when possible-dir
            ;; iterate through all possible file names
            (dolist (pair regexps)
              (let ((possible-file (concat possible-dir
                                           (funcall (cdr pair) basename))))
                (when (file-exists-p possible-file)
                  (cl-pushnew possible-file results :test 'string=)))))))
      results)))


(defun cff-process-found (found)
  "Post-processing of the FOUND list of files."
  (cond ((not found) (message "Not found"))
        ;; found one alternative
        ((and found (= (length found) 1))
         (find-file (car found)))
        ;; found several alternatives
        (t
         ;; if helm is available and cff-no-choice is true
         (if (and (not cff-no-choice) (fboundp 'helm))
             ;; use helm if available
             (let ((some-helm-source
                    '((name . "Possible alternatives")
                      (candidates . found)
                      (action . (lambda (candidate)
                                  (find-file candidate))))))
               (helm :sources '(some-helm-source)))
           ;; otherwise just pick the first file in the list
           (find-file (car found))))))


(defun cff-find-in-git (fname top-dir regexps)
  "Find the list of files in git repository based on FNAME in TOP-DIR.
Using REGEXPS to construct a list of files based on FNAME."
  (let* ((basename (file-name-base fname))
         ;; list of files to look for
         (filelist (mapcar #'(lambda (x) (funcall (cdr x) basename)) regexps))
         (fregexp (concat "/\\(" (mapconcat 'identity filelist "\\|") "\\)$"))
         ;; list of files in repo
         (git-list
          ;; contatenate all found files with the top-dir
          (mapcar #'(lambda (x) (concat top-dir x))
                  (split-string         ; split the list of files from string
                   (with-temp-buffer
                     (call-process "git" nil 't nil
                                   "ls-files" "--full-name" top-dir)
                     (buffer-string))))) ; run the command to the temp buffer
         (found nil))
    (dolist (f git-list)
      (when (string-match fregexp f)
        (push f found)))
    found))

;;;###autoload
(defun cff-find-other-file ()
  "Find the appropriate header, source or interface file for the current file."
  (interactive)
  (let* ((fname (expand-file-name (buffer-file-name)))     ; full file name
         (ftype (cff-file-type fname))  ; file type
         (fdir (file-name-directory fname)) ; directory where the file is
         ;; base file name (without extension)
         (fname-without-ext (file-name-base fname))
         (top-dir-pair (cff-top-repo-directory-for-file fname))
         (top-dir (car top-dir-pair))         ; repo top directory
         (repo-type (cdr top-dir-pair))       ; repo type
         (replacement (cff-find-replacement fname ftype))
         (regexps (make-hash-table)))
    (puthash 'header cff-source-regexps regexps)
    (puthash 'source cff-header-regexps regexps)
    (puthash 'interface cff-source-regexps regexps)
    (if (eql ftype 'unknown)
        (message "Unknown file type")
      ;; first find in closest directrories up in file hierarchy
      (let ((found
             (cond ((eql ftype 'header)
                    (cff-find-files-with-predicate top-dir fdir cff-source-dirs
                                                   #'(lambda (x)
                                                       (cff-is-source-for-header
                                                        x fname))))
                   ((eql ftype 'source)
                    (cff-find-files-with-predicate top-dir fdir cff-header-dirs
                                                   #'(lambda (x)
                                                       (cff-is-header-for-source
                                                        x fname))))
                   ((eql ftype 'interface)
                    (cff-find-files-with-predicate
                     top-dir fdir cff-source-dirs
                     #'(lambda (x)
                         (cff-is-source-for-interface x fname))))
                   (t nil)))
            ;; next try to find by replacing strings in path (like src->inc)
            (found-in-path
             (cond ((eql ftype 'header)
                    (cff-find-files-with-path fname replacement cff-source-dirs
                                              cff-source-regexps))
                   ((eql ftype 'source)
                    (cff-find-files-with-path fname replacement cff-header-dirs
                                              cff-header-regexps))
                   ((eql ftype 'interface)
                    (cff-find-files-with-path fname replacement cff-source-dirs
                                              cff-source-regexps))))
            ;; and at last find all similar in git
            (found-in-git (when (eql repo-type 'git)
                            (cff-find-in-git fname top-dir (gethash ftype
                                                                    regexps)))))
        (when found-in-path
          (dolist (f found-in-path)
            ;; they may be already in results, so push only new
            (cl-pushnew f found :test 'string=)))
        ;; add all found in git repo
        (when found-in-git
          (dolist (f found-in-git)
            ;; they may be already in results, so push only new
            (cl-pushnew f found :test 'string=)))
        (when found
          ;; process results
          (setf found (nreverse found)))
        (cff-process-found found)))))


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
;; TODO: if still not found, create a directory hierarchy replacing src with inc
;; and vice-versa
;; and try to find it there


(defun cff-find-file-in-subdir (dir criteria)
  "Find files in DIR matching CRITERIA."
  (let ((files (directory-files dir)))
    (cl-find-if criteria files)))

(defun cff-find-files-with-predicate (top-dir dir subdirs criteria)
  "Find all files with given predicate.
movig up to the TOP-DIR starting from the  DIR or its SUBDIRS.
Returns the lisf of full paths to files which complies CRITERIA."
  (cff-find-files-with-iter top-dir dir subdirs criteria nil))

(defun cff-find-files-with-iter (top-dir dir subdirs criteria acc)
  "Iterator for `cff-find-files-with-predicate'.
Argument TOP-DIR see `cff-find-files-with-predicate'.
Argument DIR see `cff-find-files-with-predicate'.
Argument SUBDIRS see `cff-find-files-with-predicate'.
Argument CRITERIA see `cff-find-files-with-predicate'.
Argument ACC accumulator."
  ;; first try to look in the dir
  (let ((found (cff-find-file-in-subdir dir criteria)))
    ;; if push the result to the accumulator
    (when found (cl-pushnew (concat dir found) acc :test 'string=)))
  ;; then look in all listed subdirs of the dir
  (let* ((fulldir (file-name-as-directory dir))
         (full-subdirs
          (cl-remove-if-not 'file-exists-p
                            (mapcar #'(lambda (d)
                                        (concat fulldir
                                                (file-name-as-directory d)))
                                    subdirs))))
    (dolist (d full-subdirs)
      (let ((fname (cff-find-file-in-subdir d criteria)))
        (when fname
          (cl-pushnew (concat d fname) acc :test 'string=)))))
  ;; now verify if we are in the top dir
  (if (string= top-dir dir)
      acc                               ; return accumulated paths
    ;; otherwise repeat with the parent directory
    (cff-find-files-with-iter top-dir (file-name-directory
                                       (directory-file-name dir))
                              subdirs criteria acc)))


(defun cff-is-header-for-source (header source)
  "Determines if the HEADER (short file name) corresponds to the SOURCE.
Note: given the full file path to the SOURCE."
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
  "Determines if the SOURCE (short file name) corresponds to the HEADER.
Note: given the full file path to the HEADER."
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
  "Determines if the SOURCE (short file name) corresponds to the HEADER.
Interface HEADER (full file path)"
  (cff-is-source-for-header source header))


(provide 'cff)
;;; cff.el ends here
