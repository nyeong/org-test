;;; org-test.el --- Literal testing tools for org-mode.  -*- lexical-binding: t; -*-
;;;
;; Copyright (C) 2025 An Nyeong
;; Author: An Nyeong <me@annyeong.me>

(require 'org)
(require 'async)

;; Public API

(defun org-test-run-current-buffer ()
 (interactive)
 (org-test-run (current-buffer)))

(defun org-test-run (target)
 "TARGET might be a buffer, path or directories."
 (cond
  ((bufferp target)
   (if noninteractive
       (org-test--run-buffer-sync target)
     (org-test--run-buffer-async target)))
  (t (error "Unsupported target type: %s" (type-of target)))))

;; Private API

(defun org-test--run-buffer-sync (buffer)
 "Find all test blocks and run synchronously those in given BUFFER."
 (with-current-buffer buffer
   (let* ((test-blocks (org-test--find-tests))
        (expect-map (org-test--find-expectations))
        (state (vector (length test-blocks) 0 0 0)))
     (message "Starting %d tests in %s..." (aref state 0) (buffer-name buffer))
     (dolist (test-block test-blocks)
       (let* ((test-name-full (org-element-property :name test-block))
              (test-name (substring test-name-full 5))
            (info (save-excursion (goto-char (org-element-property :begin test-block))
                           (org-babel-get-src-block-info)))
            (lang (nth 0 info))
            (body (org-element-property :value test-block))
            (original-params (nth 2 info))
            (params (cons '(:results . "value")
                     (assq-delete-all :results original-params)))
            (actual-result (org-test--execute-test-block lang body params)))
         (org-test--process-result state test-name actual-result expect-map)))
     (message "\nFinished. Passed: %d, Failed: %d"
           (aref state 2) (aref state 3))
     ;; Return non-zero exit code if any tests failed
     (when (> (aref state 3) 0)
       (kill-emacs 1)))))

(defun org-test--run-buffer-async (buffer)
 "Find all test blocks and run asyncly those in given BUFFER."
 (with-current-buffer buffer
   (let* ((test-blocks (org-test--find-tests))
        (expect-map (org-test--find-expectations))
        (state (vector (length test-blocks) 0 0 0)))
     (message "Starting %d async tests in %s..." (aref state 0) (buffer-name buffer))
     (dolist (test-block test-blocks)
       (let* ((test-name-full (org-element-property :name test-block))
              (test-name (substring test-name-full 5))
            (info (save-excursion (goto-char (org-element-property :begin test-block))
                           (org-babel-get-src-block-info)))
            (lang (nth 0 info))
            (body (org-element-property :value test-block))
            (original-params (nth 2 info))
            (params (cons '(:results . "value")
                     (assq-delete-all :results original-params))))
         (async-start
          `(lambda ()
            (org-test--execute-test-block ,lang ,body ',params))
          `(lambda (actual-result)
            (org-test--process-result ',state ,test-name actual-result expect-map))))))))

(defun org-test--execute-test-block (lang body params)
 "Execute a babel block with LANG, BODY, and PARAMS, then return the result string.
This is the core execution logic shared by sync and async runners."
 (let* ((lang-name lang)
      (executor (intern (format "org-babel-execute:%s" lang-name))))
   ;; Required babel language library
   (require (intern (concat "ob-" lang-name)))
   (if (fboundp executor)
      (funcall executor body params)
     (error "Babel executor for '%s' not found" lang-name))))


(defun org-test--process-result (state test-name actual-result expect-map)
 (let ((completed (1+ (aref state 1))))
   (aset state 1 completed))

 (let* ((expected-blocks (gethash test-name expect-map))
        (test-passed-p t))
   (if (not expected-blocks)
       (progn (message "-> FAIL: %s (No expectation block)" test-name) (setq test-passed-p nil))
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
                (test-type (car (last (split-string exp-name "-"))))
                (comparison-result (org-test--compare actual-result expected-string test-type)))
           (unless comparison-result
             (let* ((actual-str (if (stringp actual-result) actual-result (format "%s" actual-result)))
                    (expected-str (if (stringp expected-string) expected-string 
                                    (if (listp expected-string) 
                                        (mapconcat #'identity expected-string "") 
                                      (format "%s" expected-string)))))
               (message "-> FAIL: %s (Type: %s)" test-name test-type)
               (message "   Expected: %s" (string-trim expected-str))
               (message "   Actual:   %s" (string-trim actual-str)))
             (setq test-passed-p nil)
             (cl-return-from nil))))))

   (if test-passed-p
       (progn
         (message "-> PASS: %s" test-name)
         (aset state 2 (1+ (aref state 2))))
     (aset state 3 (1+ (aref state 3)))))

 ;; For async mode only: print summary when all tests complete
 (when (and (= (aref state 0) (aref state 1))
            (not noninteractive))
   (message "\nFinished. Passed: %d, Failed: %d"
         (aref state 2) (aref state 3))))

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
            (name-parts (split-string exp-name "-"))
            (test-name (mapconcat #'identity (butlast (cdr name-parts)) "-")))
       (push exp-block (gethash test-name expect-map '()))))
   expect-map))

(defun org-test--compare (output expected type)
 "Compare OUTPUT and EXPECTED.
Available TYPEs:
  - including
  - exact"
 (let* ((output-str (if (stringp output) output (format "%s" output)))
        (expected-str (cond
                       ((stringp expected) expected)
                       ((listp expected) (mapconcat #'identity expected ""))
                       (t (format "%s" expected))))
        (clean-output (string-trim output-str))
        (clean-expected (string-trim expected-str)))
   (cond
    ((equal type "including") (string-match-p (regexp-quote clean-expected) clean-output))
    ((equal type "exact") (equal clean-expected clean-output))
    (t (message "Warning: Unknown test type: %s" type) nil))))

(provide 'org-test)
;;; org-test.el ends here
