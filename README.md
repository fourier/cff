# C/C++ find file

## About
This extension allows to quickly switch between header and a source file with the same name located in the directory tree or repository.
It is an alternatife to ```ff-find-other-file```.

## Usage
Add the following to your .emacs file:
```elisp 
(require 'cff)
;; defines shortcut for find source/header file for the current
;; file
(add-hook 'c++-mode-hook
           '(lambda ()
              (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
(add-hook 'c-mode-hook
           '(lambda ()
              (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
```

## Algorithm

This command tries to find the file with the same name as current but another extension, i.e. if the current file has a **.c** extension, the command will try to find other file with the same basename but the possible extensions **.h**, **.hpp** etc. and vise versa. If the file is not found, it will be looked for in the subdirectories like **inc** and **include**. If not found, the procedure repeats with the upper directory.
