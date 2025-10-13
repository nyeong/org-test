;;; org-test.el --- Literal testing tools for org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 An Nyeong

;; Author: An Nyeong <me@annyeong.me>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.0"))
;; Keywords: org, testing, literate-programming
;; URL: https://github.com/annyeong/org-test

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-test provides literal testing tools for org-mode documents.
;; Write tests as named org-babel source blocks and expectations as
;; example blocks, then run tests interactively or in batch mode.
;;
;; Basic usage:
;;
;;   #+NAME: test-example
;;   #+begin_src emacs-lisp
;;   (+ 1 2)
;;   #+end_src
;;
;;   #+NAME: expect-example-exact
;;   #+begin_example
;;   3
;;   #+end_example
;;
;; Run tests with M-x org-test-run-current-buffer or:
;;   (org-test-run "file.org")
;;   (org-test-run "directory/")
;;
;; See README.md for detailed documentation.

;;; Code:

(require 'org)
(require 'compile)

;; Configuration

(defvar org-test-default-timeout 30
  "Default timeout in seconds for each test.
Set to nil to disable timeout.")

;; Faces

(defface org-test-pass-face
  '((t :foreground "green" :weight bold))
  "Face for passed tests.")

(defface org-test-fail-face
  '((t :foreground "red" :weight bold))
  "Face for failed tests.")

;; Org Test Results Mode

(define-derived-mode org-test-mode org-mode "Org-Test"
  "Major mode for Org Test results."
  (setq-local buffer-read-only t))

;; Output Functions

(defun org-test--get-results-buffer ()
  "Get or create *Org Test* buffer."
  (let ((buffer (get-buffer-create "*Org Test*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'org-test-mode)
        (org-test-mode)
        (setq buffer-read-only nil)))
    buffer))

(defun org-test--display-results-buffer ()
  "Display test results buffer."
  (display-buffer (org-test--get-results-buffer)))

(defun org-test--output (format-string &rest args)
  "Output to appropriate destination based on mode."
  (let ((text (apply #'format format-string args)))
    (if noninteractive
        (message "%s" text)
      (with-current-buffer (org-test--get-results-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert text "\n"))))))

;; Public API

;;;###autoload
(defun org-test-run-current-buffer ()
 "Run tests in the current buffer."
 (interactive)
 (org-test-run (current-buffer)))

;;;###autoload
(defun org-test-run (&rest targets)
 "Run tests on TARGETS (buffers, files, or directories).
TARGETS can be:
- Buffers
- File paths (strings ending in .org)
- Directory paths (finds all .org files recursively)
- Multiple arguments of any combination"
 (let ((buffers (org-test--normalize-targets targets))
       (total-passed 0)
       (total-failed 0)
       (total-tests 0))
   ;; Count total tests first
   (dolist (buffer buffers)
     (with-current-buffer buffer
       (setq total-tests (+ total-tests (length (org-test--find-tests))))))
   
   ;; If no tests, just message and return
   (if (= total-tests 0)
       (message "No tests found")
     ;; Has tests - setup buffer and run
     (unless noninteractive
       (with-current-buffer (org-test--get-results-buffer)
         (let ((inhibit-read-only t))
           (erase-buffer)))
       (org-test--display-results-buffer))
     
     (dolist (buffer buffers)
       (let ((result (org-test--run-buffer-sync buffer)))
         (setq total-passed (+ total-passed (car result)))
         (setq total-failed (+ total-failed (cdr result)))))
     
     (org-test--output "")
     (if noninteractive
         (org-test--output "Passed: %d, Failed: %d" total-passed total-failed)
       (progn
         (org-test--output "* Summary")
         (org-test--output "Passed: %d, Failed: %d" total-passed total-failed)))
     ;; Return non-zero exit code if any tests failed
     (when (and noninteractive (> total-failed 0))
       (kill-emacs 1)))))

;; Private API

(defun org-test--normalize-targets (targets)
 "Convert TARGETS to a list of buffers.
TARGETS can be buffers, file paths, or directory paths."
 (let ((buffers '()))
   (dolist (target targets)
     (cond
      ((bufferp target)
       (push target buffers))
      ((and (stringp target) (file-directory-p target))
       (dolist (file (org-test--find-org-files target))
         (push (org-test--file-to-buffer file) buffers)))
      ((and (stringp target) (file-exists-p target))
       (push (org-test--file-to-buffer target) buffers))
      (t (error "Invalid target: %s (not a buffer, file, or directory)" target))))
   (nreverse buffers)))

(defun org-test--file-to-buffer (file-path)
 "Load FILE-PATH into a temporary buffer and return it."
 (let ((buffer (find-file-noselect (expand-file-name file-path))))
   buffer))

(defun org-test--find-org-files (directory)
 "Recursively find all .org files in DIRECTORY."
 (let ((files '()))
   (dolist (file (directory-files-recursively directory "\\.org$"))
     (push file files))
   (nreverse files)))

(defun org-test--run-buffer-sync (buffer)
 "Find all test blocks and run synchronously those in given BUFFER.
Returns (passed . failed) cons cell."
 (with-current-buffer buffer
   (let* ((test-blocks (org-test--find-tests))
        (expect-map (org-test--find-expectations))
        (state (vector (length test-blocks) 0 0 0))
        (file-path (buffer-file-name buffer)))
     (if noninteractive
         (org-test--output "%s" (buffer-name buffer))
       (org-test--output "* %s" (buffer-name buffer)))
     (dolist (test-block test-blocks)
       (let* ((test-name-full (org-element-property :name test-block))
              (test-name (substring test-name-full 5))
              (test-begin (org-element-property :begin test-block))
              (test-line (line-number-at-pos test-begin))
            (info (save-excursion (goto-char test-begin)
                           (org-babel-get-src-block-info)))
            (lang (nth 0 info))
            (body (org-element-property :value test-block))
            (original-params (nth 2 info))
            (params (cons '(:results . "value")
                     (assq-delete-all :results original-params)))
            (eval-param (cdr (assoc :eval original-params)))
            (actual-result 
             (if (member eval-param '("no" "never"))
                 ;; :eval no - use RESULTS block
                 (org-test--get-results-block test-block)
               ;; Normal execution
               (org-test--execute-test-block lang body params))))
         (org-test--process-result state test-name-full test-name file-path actual-result expect-map)))
     ;; Return (passed . failed)
     (cons (aref state 2) (aref state 3)))))

(defun org-test--run-buffer-async (buffer)
 "Find all test blocks and run asyncly those in given BUFFER."
 (with-current-buffer buffer
   (let* ((test-blocks (org-test--find-tests))
        (expect-map (org-test--find-expectations))
        (state (vector (length test-blocks) 0 0 0))
        (file-path (buffer-file-name buffer)))
     (if noninteractive
         (org-test--output "%s" (buffer-name buffer))
       (org-test--output "* %s" (buffer-name buffer)))
     (dolist (test-block test-blocks)
       (let* ((test-name-full (org-element-property :name test-block))
              (test-name (substring test-name-full 5))
              (test-begin (org-element-property :begin test-block))
              (test-line (line-number-at-pos test-begin))
            (info (save-excursion (goto-char test-begin)
                           (org-babel-get-src-block-info)))
            (lang (nth 0 info))
            (body (org-element-property :value test-block))
            (original-params (nth 2 info))
            (params (cons '(:results . "value")
                     (assq-delete-all :results original-params)))
            (eval-param (cdr (assoc :eval original-params))))
         (if (member eval-param '("no" "never"))
             ;; :eval no - use RESULTS block (sync only)
             (let ((actual-result (org-test--get-results-block test-block)))
               (org-test--process-result state test-name-full test-name file-path actual-result expect-map))
           ;; Normal async execution
           (async-start
            `(lambda ()
              (org-test--execute-test-block ,lang ,body ',params))
            `(lambda (actual-result)
              (org-test--process-result ',state ,test-name-full ,test-name ,file-path actual-result expect-map)))))))))

(defun org-test--get-results-block (src-block)
  "Get the RESULTS block content following SRC-BLOCK.
Used when :eval no is specified."
  (save-excursion
    (goto-char (org-element-property :end src-block))
    (when (re-search-forward "^[ \t]*#\\+RESULTS:" 
                             (save-excursion (outline-next-heading) (point))
                             t)
      (forward-line 1)
      (let ((results-elem (org-element-at-point)))
        (pcase (org-element-type results-elem)
          ('example-block (org-element-property :value results-elem))
          ('fixed-width (org-element-property :value results-elem))
          ('paragraph (org-element-interpret-data (org-element-contents results-elem)))
          (_ (error "Unsupported RESULTS block type: %s" (org-element-type results-elem))))))))

(defun org-test--execute-test-block (lang body params)
 "Execute a babel block with LANG, BODY, and PARAMS, then return the result string.
Respects `org-test-default-timeout' for execution timeout."
 (let* ((lang-name lang)
        (executor (intern (format "org-babel-execute:%s" lang-name))))
   ;; Required babel language library
   (require (intern (concat "ob-" lang-name)))
   (if (fboundp executor)
       (if org-test-default-timeout
           (with-timeout (org-test-default-timeout
                          (error "Test timeout after %d seconds" org-test-default-timeout))
             (funcall executor body params))
         (funcall executor body params))
     (error "Babel executor for '%s' not found" lang-name))))


(defun org-test--process-result (state test-name-full test-name file-path actual-result expect-map)
 (let ((completed (1+ (aref state 1))))
   (aset state 1 completed))

 (let* ((expected-blocks (gethash test-name expect-map))
        (test-passed-p t))
   (if (not expected-blocks)
       (progn 
         (if noninteractive
             (org-test--output "  %s %s (No expectation block)" 
                              (propertize "✗" 'face 'org-test-fail-face)
                              test-name)
           (org-test--output "** %s [[file:%s::%s][%s]] (No expectation block)" 
                            (propertize "✗" 'face 'org-test-fail-face)
                            file-path test-name-full test-name))
         (setq test-passed-p nil))
     ;; Print test name
     (if noninteractive
         (org-test--output "  %s" test-name)
       (org-test--output "** [[file:%s::%s][%s]]" file-path test-name-full test-name))
     (cl-block nil
       (dolist (exp-block expected-blocks)
         (let* ((exp-name (org-element-property :name exp-block))
                (exp-type (org-element-type exp-block))
                (expected-string (cond
                                  ((eq exp-type 'example-block)
                                   (org-element-property :value exp-block))
                                  ((eq exp-type 'fixed-width)
                                   (org-element-property :value exp-block))
                                  (t (org-element-interpret-data (org-element-contents exp-block)))))
                ;; Extract test type from expect name (same logic as find-expectations)
                (test-type (cond
                            ((string-suffix-p "-not-including" exp-name) "not-including")
                            ((string-suffix-p "-excluding" exp-name) "excluding")
                            ((string-suffix-p "-contains-all" exp-name) "contains-all")
                            ((string-suffix-p "-including" exp-name) "including")
                            ((string-suffix-p "-matching" exp-name) "matching")
                            ((string-suffix-p "-matches" exp-name) "matches")
                            ((string-suffix-p "-exact" exp-name) "exact")
                            (t "unknown")))
                (comparison-result (org-test--compare actual-result expected-string test-type)))
           (if comparison-result
               (if noninteractive
                   (org-test--output "    %s %s" 
                                    (propertize "✓" 'face 'org-test-pass-face)
                                    test-type)
                 (org-test--output "   - %s [[file:%s::%s][%s]]" 
                                  (propertize "✓" 'face 'org-test-pass-face)
                                  file-path exp-name test-type))
             (let* ((actual-str (if (stringp actual-result) actual-result (format "%s" actual-result)))
                    (expected-str (if (stringp expected-string) expected-string 
                                    (if (listp expected-string) 
                                        (mapconcat #'identity expected-string "") 
                                      (format "%s" expected-string)))))
               (if noninteractive
                   (progn
                     (org-test--output "    %s %s" 
                                      (propertize "✗" 'face 'org-test-fail-face)
                                      test-type)
                     (org-test--output "      Expected: %s" (string-trim expected-str))
                     (org-test--output "      Actual:   %s" (string-trim actual-str)))
                 (org-test--output "   - %s [[file:%s::%s][%s]]" 
                                  (propertize "✗" 'face 'org-test-fail-face)
                                  file-path exp-name test-type)
                 (org-test--output "     - Expected: %s" (string-trim expected-str))
                 (org-test--output "     - Actual:   %s" (string-trim actual-str)))
               (setq test-passed-p nil)
               (cl-return-from nil)))))))

   (if test-passed-p
       (aset state 2 (1+ (aref state 2)))
     (aset state 3 (1+ (aref state 3))))))


(defun org-test--find-tests ()
 "Find all test blocks."
 (org-element-map (org-element-parse-buffer) 'src-block
   (lambda (block)
     (let ((name (org-element-property :name block)))
       (when (and name (string-prefix-p "test-" name)) block)))))

(defun org-test--find-expectations ()
 "Find all expect blocks, return (test-name . expect-list)."
 (let ((expect-map (make-hash-table :test 'equal)))
   (dolist (exp-block (org-element-map (org-element-parse-buffer) '(example-block fixed-width)
                 (lambda (exp)
                   (let ((exp-name (org-element-property :name exp)))
                     (when (and exp-name (string-prefix-p "expect-" exp-name))
                       exp)))))
     (let* ((exp-name (org-element-property :name exp-block))
            ;; Remove "expect-" prefix
            (without-prefix (substring exp-name 7))
            ;; Extract test name by checking known postfixes (longest first)
            (test-name (cond
                        ((string-suffix-p "-not-including" without-prefix)
                         (substring without-prefix 0 -14))
                        ((string-suffix-p "-excluding" without-prefix)
                         (substring without-prefix 0 -10))
                        ((string-suffix-p "-contains-all" without-prefix)
                         (substring without-prefix 0 -13))
                        ((string-suffix-p "-including" without-prefix)
                         (substring without-prefix 0 -10))
                        ((string-suffix-p "-matching" without-prefix)
                         (substring without-prefix 0 -9))
                        ((string-suffix-p "-matches" without-prefix)
                         (substring without-prefix 0 -8))
                        ((string-suffix-p "-exact" without-prefix)
                         (substring without-prefix 0 -6))
                        (t without-prefix))))
       (push exp-block (gethash test-name expect-map '()))))
   expect-map))

(defun org-test--compare (output expected type)
 "Compare OUTPUT and EXPECTED.
Available TYPEs:
  - including: output contains expected text
  - exact: output matches exactly
  - excluding / not-including: output must NOT contain expected text
  - contains-all: output contains all lines from expected (order-independent)
  - matches / matching: output matches expected regex pattern"
 (let* ((output-str (if (stringp output) output (format "%s" output)))
        (expected-str (cond
                       ((stringp expected) expected)
                       ((listp expected) (mapconcat #'identity expected ""))
                       (t (format "%s" expected))))
        (clean-output (string-trim output-str))
        (clean-expected (string-trim expected-str)))
   (cond
    ((equal type "including") 
     (string-match-p (regexp-quote clean-expected) clean-output))
    ((equal type "exact") 
     (equal clean-expected clean-output))
    ((or (equal type "excluding") (equal type "not-including"))
     (not (string-match-p (regexp-quote clean-expected) clean-output)))
    ((equal type "contains-all")
     (let ((expected-lines (split-string clean-expected "\n" t)))
       (cl-every (lambda (line)
                   (string-match-p (regexp-quote (string-trim line)) clean-output))
                 expected-lines)))
    ((or (equal type "matches") (equal type "matching"))
     (string-match-p clean-expected clean-output))
    (t (message "Warning: Unknown test type: %s" type) nil))))

(provide 'org-test)
;;; org-test.el ends here
