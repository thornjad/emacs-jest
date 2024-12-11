;;; Jest.el --- helpers to run jest -*- lexical-binding: t; -*-

;; Author: Jade Michael Thornton
;; Copyright (c) 2024 Jade Michael Thornton
;; Based on work by Edmund Miller and Hyeonjun Park
;; URL:  https://github.com/thornjad/emacs-jest/
;; Version: 0.2.0
;; Keywords: tools
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (magit-popup "2.13.3") (s "1.13.0") (js2-mode "20231224"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This package provides helpers to run jest.
;;
;; The main command is jest-popup, which will show a dispatcher menu, making it easy to change
;; various options and switches, and then run jest using one of the actions.
;; - jest-file (current file)
;; - jest-test (closest it, test or describe)
;; - jest-last-failed (rerun previous failures)
;; - jest-repeat (repeat last invocation)
;;
;; Each of these commands (except jest-repeat) also has an inspection version, `jest-file-inspect',
;; etc. which enable Node inspect-brk mode.
;;
;; A prefix argument causes the generated command line to be offered for editing, and various
;; customization options influence how some of the commands work.

;;; Code:

(require 'dash)
(require 's)

(require 'comint)
(require 'compile)
(require 'js2-mode)
(require 'magit-popup)

(defgroup jest nil
  "Jest integration"
  :group 'js
  :prefix "jest-")

(defcustom jest-confirm nil
  "Whether to edit the command in the minibuffer before execution.

By default, jest will be executed without showing a minibuffer prompt.
This can be changed on a case by case basis by using a prefix argument
\(\\[universal-argument]\) when invoking a command.

When t, this toggles the behaviour of the prefix argument."
  :group 'jest
  :type 'boolean)

(defcustom jest-executable "npx --no-install jest"
  "The name of the jest executable."
  :group 'jest
  :type 'string)

(defcustom jest-inspect-executable "npx --node-options=--inspect-brk --no-install jest"
  "The Jest executable with inspect option passed to Node."
  :group 'jest
  :type 'string)

(defcustom jest-setup-hook nil
  "Hooks to run before a jest process starts."
  :group 'jest
  :type 'hook)

(defcustom jest-started-hook nil
  "Hooks to run after a jest process starts."
  :group 'jest
  :type 'hook)

(defcustom jest-finished-hook nil
  "Hooks to run after a jest process finishes."
  :group 'jest
  :type 'hook)

(defcustom jest-buffer-name "*jest*"
  "Name of the jest output buffer."
  :group 'jest
  :type 'string)

(defcustom jest-project-name-in-buffer-name t
  "Whether to include the project name in the buffer name.

This is useful when working on multiple projects simultaneously."
  :group 'jest
  :type 'boolean)

(defvar jest--history nil
  "History for jest invocations.")

(defvar jest--project-last-command (make-hash-table :test 'equal)
  "Last executed command lines, per project.")

(defvar-local jest--current-command nil
  "Current command; used in jest-mode buffers.")


;; internal helpers

(fmakunbound 'jest-mode)
(makunbound 'jest-mode)

(define-derived-mode jest-mode
  comint-mode "jest"
  "Major mode for jest sessions (derived from comint-mode)."
  (make-variable-buffer-local 'comint-prompt-read-only)
  (setq-default comint-prompt-read-only nil)
  (setq buffer-read-only t)
  (compilation-setup t))

(cl-defun jest--run (&key args file testname edit inspect)
  "Run jest for the given arguments."
  (let ((popup-arguments args)
	      command)
    (setq args (jest--transform-arguments args))
    (when (and file (file-name-absolute-p file))
      (setq file (jest--relative-file-name file)))


    (when inspect
      (setq args (-snoc args "--runInBand")))
    (when file
      (setq args (-snoc args (shell-quote-argument file))))
    (when testname
      (setq args (-snoc args "--testNamePattern" (shell-quote-argument testname))))

    (let* ((executable (if inspect jest-inspect-executable jest-executable))
           (args (cons executable args))
           (command (s-join " " args)))
      (jest--run-command
       :command command
       :popup-arguments popup-arguments
       :edit edit))))

(cl-defun jest--run-command (&key command popup-arguments edit)
  "Run a jest command line."
  (save-some-buffers)
  (let* ((default-directory (project-root (project-current))))
    (when jest-confirm
      (setq edit (not edit)))
    (when edit
      (setq command
            (read-from-minibuffer
             "Command: "
             command nil nil 'jest--history)))
    (add-to-history 'jest--history command)
    (setq jest--history (-uniq jest--history))
    (puthash (project-root (project-current)) command
             jest--project-last-command)
    (jest--run-as-comint
     :command command
     :popup-arguments popup-arguments)))

(cl-defun jest--run-as-comint (&key command popup-arguments)
  "Run a jest comint session for COMMAND."
  (let* ((buffer (jest--get-buffer))
         (process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (when (comint-check-proc buffer)
          (unless (or compilation-always-kill (yes-or-no-p "Kill running jest process?"))
            (user-error "Aborting; jest still running")))
        (when process (delete-process process))
        (erase-buffer)
        (unless (eq major-mode 'jest-mode)
          (jest-mode))
        (compilation-forget-errors)
        (insert (format "cwd: %s\ncmd: %s\n\n" default-directory command))
        (make-local-variable 'jest-arguments)
        (setq jest--current-command command
              jest-arguments popup-arguments)
        (run-hooks 'jest-setup-hook)
        (make-comint-in-buffer "jest" buffer "sh" nil "-c" command)
        (run-hooks 'jest-started-hook)
        (setq process (get-buffer-process buffer))
        (set-process-sentinel process #'jest--process-sentinel)
        (display-buffer buffer)))))

(defun jest--closest-test-name ()
  (let ((text (string-join
               (reverse (split-string (buffer-substring-no-properties 1 (line-end-position)) "\n"))
               "\n")))
    (when (string-match "^[\t\s]*\\(?:test\\|it\\|describe\\)\(\\(\'\\|\"\\)\\(.*?\\)\\1" text)
      (match-string 2 text))))

(defun jest--get-buffer ()
  "Get a create a suitable compilation buffer."
  (magit-with-pre-popup-buffer
    (if (eq major-mode 'jest-mode)
        (current-buffer)  ;; re-use buffer
      (let ((name jest-buffer-name))
        (when jest-project-name-in-buffer-name
          (setq name (format "%s<%s>" name (project-name (project-current)))))
        (get-buffer-create name)))))

(defun jest--process-sentinel (proc _state)
  "Process sentinel helper to run hooks after PROC finishes."
  (with-current-buffer (process-buffer proc)
    (run-hooks 'jest-finished-hook)))

(defun jest--transform-arguments (args)
  "Transform ARGS so that jest understands them."
  (-->
   args
   (jest--switch-to-option it "--color" "--color=yes" "--color=no")
   (jest--quote-string-option it "-k")
   (jest--quote-string-option it "-m")))

(defun jest--switch-to-option (args name on-replacement off-replacement)
  "Look in ARGS for switch NAME and turn it into option with a value.

When present ON-REPLACEMENT is substituted, else OFF-REPLACEMENT is appended."
  (if (-contains-p args name)
      (-replace name on-replacement args)
    (-snoc args off-replacement)))

(defun jest--quote-string-option (args option)
  "Quote all values in ARGS with the prefix OPTION as shell strings."
  (--map-when
   (s-prefix-p option it)
   (let ((s it))
     (--> s
          (substring it (length option))
          (s-trim it)
          (shell-quote-argument it)
          (format "%s %s" option it)))
   args))

(defun jest--choose-traceback-style (prompt _value)
  "Helper to choose a jest traceback style using PROMPT."
  (completing-read
   prompt '("long" "short" "line" "native" "no") nil t))


(defun jest--make-test-name (func)
  "Turn function name FUNC into a name (hopefully) matching its test name.

Example: ‘MyABCThingy.__repr__’ becomes ‘test_my_abc_thingy_repr’."
  (-->
   func
   (s-replace "." "_" it)
   (s-snake-case it)
   (s-replace-regexp "_\+" "_" it)
   (s-chop-suffix "_" it)
   (s-chop-prefix "_" it)
   (format "test_%s" it)))


;; file/directory helpers

(defun jest--relative-file-name (file)
  "Make FILE relative to the project root."
  ;; Note: setting default-directory gives different results
  ;; than providing a second argument to file-relative-name.
  (let ((default-directory (project-root (project-current))))
    (file-relative-name file)))

;; functions to inspect/navigate the javascript source code
(defun jest--current-testname ()
  "Return the testname where pointer is located.

Testname is defined by enclosing ~describe~ calls and ~it~/~test~ calls."
  (let* ((calls (jest--list-named-calls-upwards))
         (testname ""))
    (dolist (call calls)
      ;; call is the node for the function, function name must be extracted
      ;; from its target node
      (let ((funcname (js2-name-node-name (js2-call-node-target call))))
        (when (member funcname '("it" "test" "describe"))
          (let ((funcparam (jest--function-first-param-string call)))
            (setq testname (format "%s %s" funcparam testname))))))
    (unless (string= testname "") (string-trim testname))))

(defun jest--list-named-calls-upwards ()
  "List functions call nodes where function has a name.

This goes from pointer position upwards."
  (save-excursion
    ;; enter the test function if the point is before it
    ;; separated only by whitespace, e.g.
    (skip-chars-forward "[:blank:]")
    (let* ((nodes ())
           (node (js2-node-at-point)))
      (while (not (js2-ast-root-p node))
        (when (js2-call-node-p node)
          (let ((target (js2-call-node-target node)))
            (when (js2-name-node-p target)
              (setq nodes (append nodes (list node))))))
        (setq node (js2-node-parent node)))
      nodes)))

(defun jest--function-first-param-string (node)
  "Get the first param from the function call"
  (let ((first-param (car (js2-call-node-args node))))
    (when (js2-string-node-p first-param)
      (js2-string-node-value first-param))))

(defun jest-clear-buffer-after-test-end (inserted-string)
  (let ((test-end-regex
         ".*?Test Suites:.+\nTests:  .+\nSnapshots: .+\nTime:  .+\nRan all test suites.+\n.*?")
        (inhibit-read-only t))
    (when (and (s-contains? "*jest*"
                            (buffer-name))
               (s-matches? test-end-regex (buffer-string)))
      (beginning-of-buffer)
      (comint-clear-buffer))
    inserted-string))
(add-hook 'comint-preoutput-filter-functions #'jest-clear-buffer-after-test-end)



;; Clean up jest-popup if it's already run before
(fmakunbound 'jest-popup)
(makunbound 'jest-popup)

;; TODO switch to transient
;;;###autoload (autoload 'jest-popup "jest" nil t)
(magit-define-popup jest-popup
  "Show popup for running jest."
  'jest
  :switches
  '((?b "bail" "--bail")
    (?c "colors" "--colors" t)
    (?C "coverage" "--coverage")
    (?D "debug jest config" "--debug")
    (?e "expand" "--expand")
    (?f "force exit" "--forceExit")
    (?l "last commit" "--lastCommit")
    (?o "only changed" "--onlyChanged")
    (?s "silent" "--silent")
    (?v "verbose" "--verbose")
    (?w "watch" "--watch")
    (?W "watch all" "--watchAll"))
  :options
  '((?c "config file" "--config=")
    (?k "only names matching expression" "-t")
    (?p "only files matching expression" "--testPathPattern ")
    (?P "only files not matching expression" "--testPathIgnorePatterns ")
    (?o "output file" "--outputFile=")
    (?x "exit after N failures or errors" "--maxfail="))
  :actions
  '("Run tests"
    (?t "Test all" jest)
    (?x "Test last-failed" jest-last-failed)
    "Run tests for current context"
    (?f "Test file" jest-file-dwim)
    (?F "Test this file  " jest-file)
    (?d "Test nearest it/describe " jest-test)
    "Repeat tests"
    (?r "Repeat last test run" jest-repeat))
  :max-action-columns 2
  :default-action 'jest-repeat)

;;;###autoload
(defun jest-file (file &optional args inspect)
  "Run jest on FILE, using ARGS.

Additional ARGS are passed along to jest.
With a prefix argument, allow editing."
  (interactive
   (list
    (buffer-file-name)
    (jest-arguments)))
  (jest--run
   :args args
   :file file
   :inspect inspect
   :edit current-prefix-arg))

;;;###autoload
(defun jest-file-inspect (file &optional args)
  (interactive
   (list
    (buffer-file-name)
    (jest-arguments)))
  (jest-file file args t))

;;;###autoload
(defun jest-test (file testname &optional args inspect)
  "Run jest on the test function where pointer is located.

When pointer is not inside a test function jest is run on the whole file."
  (interactive
   (list (buffer-file-name) (jest--closest-test-name) (jest-arguments) nil))
  (jest--run
   :args args
   :file file
   :inspect inspect
   :testname testname))

;;;###autoload
(defun jest-test-inspect (file testname &optional args)
  (interactive
   (list (buffer-file-name) (jest--closest-test-name) (jest-arguments)))
  (jest-test file testname args t))

;;;###autoload
(defun jest-last-failed (&optional args inspect)
  "Run jest, only executing previous test failures.

Additional ARGS are passed along to jest.
With a prefix argument, allow editing."
  (interactive (list (jest-arguments)))
  (jest--run
   :args (-snoc args "--last-failed")
   :inspect inspect
   :edit current-prefix-arg))

;;;###autoload
(defun jest-last-failed-inspect (&optional args)
  (interactive (list (jest-arguments)))
  (jest-last-failed args t))

;;;###autoload
(defun jest-repeat ()
  "Run jest with the same argument as the most recent invocation.

With a prefix ARG, allow editing."
  (interactive)
  (let ((command (gethash
                  (project-root (project-current))
                  jest--project-last-command)))
    (when jest--current-command
      ;; existing jest-mode buffer; reuse command
      (setq command jest--current-command))
    (unless command
      (user-error "No previous jest run for this project"))
    (jest--run-command
     :command command
     :popup-arguments jest-arguments
     :edit current-prefix-arg)))

(provide 'jest)

;;; jest.el ends here
