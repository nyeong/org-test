;;; call-graph.el --- Generate call graph for org-test -*- lexical-binding: t; -*-
;; AI Generated wowowow I love claude

;; Generate call graph using s-expression parsing

(defun org-test--extract-function-definitions (file)
  "Extract all function definitions and their bodies from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((functions '()))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (listp form)
                         (eq (car form) 'defun))
                (let* ((name (symbol-name (nth 1 form)))
                       (body (nthcdr 3 form)))
                  (when (string-prefix-p "org-test-" name)
                    (push (cons name body) functions))))))
        (end-of-file nil))
      (nreverse functions))))

(defun org-test--extract-calls-from-sexp (sexp)
  "Extract function calls from SEXP recursively."
  (let ((calls '()))
    (cond
     ;; Skip atoms (strings, numbers, symbols that aren't in function position)
     ((not (listp sexp)) nil)
     ;; Handle cons cells (dotted pairs)
     ((and (consp sexp) (not (listp (cdr sexp)))) nil)
     ;; Handle proper lists starting with a symbol (function call)
     ((and (consp sexp) (symbolp (car sexp)))
      (let ((func-name (symbol-name (car sexp))))
        (when (string-prefix-p "org-test-" func-name)
          (push func-name calls)))
      (dolist (elem (cdr sexp))
        (when (listp elem)
          (setq calls (append calls (org-test--extract-calls-from-sexp elem))))))
     ;; Handle lists (recursively process each element)
     ((listp sexp)
      (dolist (elem sexp)
        (when (listp elem)
          (setq calls (append calls (org-test--extract-calls-from-sexp elem)))))))
    calls))

(defun org-test--extract-function-calls (body)
  "Extract all org-test function calls from function BODY."
  (let ((calls '()))
    (dolist (form body)
      (setq calls (append calls (org-test--extract-calls-from-sexp form))))
    (delete-dups calls)))

(defun org-test--generate-dot (file output)
  "Generate DOT file from FILE and save to OUTPUT."
  (let* ((func-defs (org-test--extract-function-definitions file))
         (edges '())
         (all-functions (mapcar #'car func-defs)))
    
    ;; Extract all edges (function calls)
    (dolist (func-def func-defs)
      (let* ((func-name (car func-def))
             (body (cdr func-def))
             (calls (org-test--extract-function-calls body)))
        (dolist (call calls)
          (when (and (member call all-functions)
                     (not (equal call func-name)))
            (push (cons func-name call) edges)))))
    
    (setq edges (delete-dups edges))
    
    ;; Generate DOT format
    (with-temp-file output
      (insert "digraph org_test {\n")
      (insert "  rankdir=TB;\n")
      (insert "  node [shape=box, style=rounded];\n")
      (insert "  graph [fontname=\"Arial\", fontsize=10, pad=0.5];\n")
      (insert "  node [fontname=\"Arial\", fontsize=9];\n")
      (insert "  edge [fontname=\"Arial\", fontsize=8];\n\n")
      
      ;; Classify nodes
      (insert "  // Public API\n")
      (insert "  node [fillcolor=\"#E8F5E9\", style=\"rounded,filled\"];\n")
      (dolist (func all-functions)
        (when (not (string-match "--" func))
          (insert (format "  \"%s\";\n" func))))
      
      (insert "\n  // Private API\n")
      (insert "  node [fillcolor=\"#FFF3E0\", style=\"rounded,filled\"];\n")
      (dolist (func all-functions)
        (when (string-match "--" func)
          (insert (format "  \"%s\";\n" func))))
      
      (insert "\n  // Edges\n")
      (dolist (edge edges)
        (insert (format "  \"%s\" -> \"%s\";\n" (car edge) (cdr edge))))
      
      (insert "}\n"))
    
    (message "Generated DOT file: %s" output)
    (message "Total functions: %d" (length all-functions))
    (message "Total edges: %d" (length edges))))

;; Main execution
(let ((source-file (or (getenv "SOURCE_FILE") "org-test.el"))
      (dot-file (or (getenv "DOT_FILE") "call-graph.dot")))
  (org-test--generate-dot source-file dot-file))

