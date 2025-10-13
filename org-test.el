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
   (org-test--run-buffer-async target))
  (t (error "Unsupported target type: %s" (type-of target)))))

;; Private API


(defun org-test--run-buffer-async (buffer)
 "Find all test blocks and run asyncly those in given BUFFER."
 (with-current-buffer buffer
   (let* ((test-blocks (org-test--find-tests))
        (expect-map (org-test--find-expectations))
        (state (vector (length test-blocks) 0 0 0)))
     (message "Starting %d async tests in %s..." (aref state 0) (buffer-name buffer))
     (dolist (test-block test-blocks)
       (let* ((test-name (org-element-property :name test-block))
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
     (dolist (exp-block expected-blocks)
       (let* ((exp-name (org-element-property :name exp-block))
            (expected-string (org-element-contents exp-block))
            (test-type (car (last (split-string exp-name "-")))))
         (unless (org-test--compare actual-result expected-string test-type)
           (message "-> FAIL: %s (Type: %s)" test-name test-type)
           (setq test-passed-p nil)
           (cl-return-from nil))))))

 (if test-passed-p
    (progn
     (message "-> PASS: %s" test-name)
     (aset state 2 (1+ (aref state 2))))
   (aset state 3 (1+ (aref state 3))))

 (when (= (aref state 0) (aref state 1)) 
   (message "Finished all async tests. Passed: %d, Failed: %d"
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
     (let* ((name-parts (split-string (org-element-property :name exp-block) "-"))
          (test-name (mapconcat #'identity (butlast (cdr name-parts)) "-")))
       (push exp-block (gethash test-name expect-map '()))))
   expect-map))

(defun org-test--compare (output expected type)
 "Compare OUTPUT and EXPECTED.
Available TYPEs:
  - including
  - exact"
 (let ((clean-output (string-trim output))
     (clean-expected (string-trim expected)))
   (cond
    ((equal type "including") (string-match-p (regexp-quote clean-expected) clean-output))
    ((equal type "exact") (equal clean-expected clean-output))
    (t (message "Warning: Unknown test type: %s" type) nil))))

(provide 'org-test)
;;; org-test.el ends here
