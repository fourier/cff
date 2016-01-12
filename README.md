# C/C++ find other file

## About
This extension allows to quickly switch between C/C++ header and a source file with the same name located in the directory tree or a git repository.
It is an alternative to `ff-find-other-file`.

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

1. This command tries to find the file with the same name as current but another extension, i.e. if the current file has a **.cc** extension, the command will try to find other file with the same basename but the possible extensions **.h**, **.hpp** etc. and vise versa.
1. If the file is not found, it will be looked for in the subdirectories like **inc** and **include**.
1. If not found, the procedure repeats with the upper directory.
1. If still nothing found and the current VCS is git, try to search in the whole git repository.
1. If still nothing, try to find a corresponding file in git repository which contains basename as a part.

If multiple files found, present a selection to the user.
When `helm` is enabled, present selection using `helm`, otherwise (or when the variable `cff-use-helm-choice` set to `t`) use the custome mode to present selection.

The file selected once is stored in the cache; to get the selection choice again one should prefix the command, i.e. `C-u M-o` if the command binded to `M-o`.
