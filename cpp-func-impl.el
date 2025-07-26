;;; cpp-func-impl.el --- Generate C++ method implementations from declarations -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Free Software Foundation, Inc.
;;
;; Author: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Maintainer: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; URL: https://github.com/dheerajshenoy/cpp-func-impl.el
;; Version: 0.1.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: c++, tools, treesitter
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides functionality to automatically generate C++ method
;; implementations from class declarations using tree-sitter. It supports:
;; - Single method implementation
;; - All methods implementation
;; - Selected methods implementation
;; - Pure virtual method concrete class generation
;; - Template methods
;; - Nested classes
;; - Customizable comment generation

;;; Code:

(require 'treesit)

;;; Customization

(defgroup cpp-func-impl nil
  "Generate C++ method implementations from class declarations using tree-sitter."
  :group 'tools
  :prefix "cpp-func-impl-")

(defcustom cpp-func-impl-comment-string "// TODO: implement `%m`"
  "Comment inserted in the function body.

You can use format specifiers in the string to inject method or
timestamp information automatically.

Format specifiers:
%c - Class name
%m - Method name
%d - Current date (YYYY-MM-DD)
%t - Current time (HH:MM)

These will be expanded dynamically when the implementation stub is inserted."
  :type 'string
  :group 'cpp-func-impl)

(defcustom cpp-func-impl-auto-indent t
  "Whether to automatically indent generated implementations."
  :type 'boolean
  :group 'cpp-func-impl)

(defcustom cpp-func-impl-add-newlines t
  "Whether to add newlines around generated implementations."
  :type 'boolean
  :group 'cpp-func-impl)

;;; Utility Functions

(defun cpp-func-impl--ensure-cpp-treesit ()
  "Ensure tree-sitter C++ support is available."
  (unless (treesit-available-p)
    (user-error "Tree-sitter is not available"))
  (unless (treesit-language-available-p 'cpp)
    (user-error "Tree-sitter C++ support is not available")))

(defun cpp-func-impl--safe-node-text (node &optional no-property)
  "Safely get text from NODE, returning empty string if node is nil.

If NO-PROPERTY is t, no property text specifiers are returned with the node."
  (if node
      (treesit-node-text node no-property)
    ""))

(defun cpp-func-impl--find-class-node (&optional node)
  "Find the enclosing class_specifier node from NODE or point."
  (let ((current-node (or node (treesit-node-at (point)))))
    (treesit-parent-until current-node
                          (lambda (n)
                            (string= (treesit-node-type n) "class_specifier")))))

(defun cpp-func-impl--get-qualified-class-name (node)
  "Return the fully-qualified C++ class name by walking up from NODE."
  (let ((names '())
        (current node))
    (while current
      (when (member (treesit-node-type current)
                    '("class_specifier" "struct_specifier" "union_specifier"))
        (when-let* ((name-node (treesit-node-child-by-field-name current "name")))
          (push (cpp-func-impl--safe-node-text name-node) names)))
      (setq current (treesit-node-parent current)))
    (if names
        (string-join names "::")
      nil)))  ;; Return nil instead of empty string for clarity

(defun cpp-func-impl--format-comment (class-name method-name)
  "Format the comment string using format specifiers.

Takes CLASS-NAME and METHOD-NAME as arguments.

Valid format specifiers:
%c - Class name
%m - Method name
%d - Current date (YYYY-MM-DD)
%t - Current time (HH:MM)"
  (let* ((now (current-time))
         (date (format-time-string "%F" now))
         (time (format-time-string "%R" now)))
    (replace-regexp-in-string
     "%[cmdt]"
     (lambda (match)
       (pcase match
         ("%c" class-name)
         ("%m" method-name)
         ("%d" date)
         ("%t" time)
         (_ match)))
     cpp-func-impl-comment-string)))

(defun cpp-func-impl--trim-virtual-specifiers (text)
  "Remove 'final', 'override' keywords from TEXT."
  (when text
    (string-trim
     (replace-regexp-in-string
      "[ \t]+" " "
      (replace-regexp-in-string
       "\\b\\(override\\|final\\)\\b" ""
       text)))))

;;; Method Discovery Functions

(defun cpp-func-impl--get-methods (&optional class-node)
  "Return a list of all method declaration nodes in the class.

If CLASS-NODE is provided, search within that class, otherwise
use the class containing point."
  (cpp-func-impl--ensure-cpp-treesit)
  (let ((target-class (or class-node (cpp-func-impl--find-class-node))))
    (unless target-class
      (user-error "Not inside a class declaration"))

    (let* ((body (treesit-node-child-by-field-name target-class "body"))
           (methods '()))
      (when body
        (let ((decls (treesit-filter-child
                      body
                      (lambda (n)
                        (member (treesit-node-type n)
                                '("field_declaration" "template_declaration")))
                      t)))
          (dolist (decl decls)
            (when-let* ((func-decl (treesit-search-subtree
                                    decl
                                    (lambda (n)
                                      (string= (treesit-node-type n) "function_declarator")))))
              (push func-decl methods)))))
      (nreverse methods))))

(defun cpp-func-impl--get-virtual-methods (&optional class-node)
  "Return a list of all virtual method declarator nodes in the class.

If CLASS-NODE is provided, get virtual methods from that class."
  (cpp-func-impl--ensure-cpp-treesit)
  (let ((target-class (or class-node (cpp-func-impl--find-class-node))))
    (unless target-class
      (user-error "Not inside a class declaration"))

    (let* ((body (treesit-node-child-by-field-name target-class "body"))
           (method-nodes '()))
      (when body
        (dolist (child (treesit-filter-child body #'identity t))
          (when (member (treesit-node-type child) '("field_declaration" "template_declaration"))
            (when (treesit-filter-child child
                                        (lambda (n)
                                          (string= (treesit-node-type n) "virtual")))
              (when-let* ((func-decl
                           (treesit-search-subtree child
                                                   (lambda (n)
                                                     (string= (treesit-node-type n) "function_declarator")))))
                (push func-decl method-nodes))))))
      (nreverse method-nodes))))

(defun cpp-func-impl--get-pure-virtual-methods (&optional class-node)
  "Return a list of all pure virtual method declarator nodes in the class.

If CLASS-NODE is provided, get pure virtual methods from the class."
  (cpp-func-impl--ensure-cpp-treesit)
  (let ((target-class (or class-node (cpp-func-impl--find-class-node))))
    (unless target-class
      (user-error "Not inside a class declaration"))

    (let* ((body (treesit-node-child-by-field-name target-class "body"))
           (method-nodes '()))
      (when body
        (dolist (child (treesit-filter-child body #'identity t))
          (when (member (treesit-node-type child) '("field_declaration" "template_declaration"))
            (let ((has-virtual
                   (treesit-filter-child child
                                         (lambda (n)
                                           (string= (treesit-node-type n) "virtual"))))
                  (has-eq-zero
                   (treesit-search-subtree child
                                           (lambda (n)
                                             (and (string= (treesit-node-type n) "number_literal")
                                                  (string= (treesit-node-text n) "0"))))))
              (when (and has-virtual has-eq-zero)
                (when-let* ((func-decl
                             (treesit-search-subtree child
                                                     (lambda (n)
                                                       (string= (treesit-node-type n) "function_declarator")))))
                  (push func-decl method-nodes)))))))
      (nreverse method-nodes))))

;;; Method Information Extraction

(defun cpp-func-impl--extract-return-type (field-decl func-decl)
  "Reconstruct full return type including pointer/reference decorations.

FIELD-DECL is the field_declaration node and FUNC-DECL is the
function_declaration node obtained from tree-sitter."
  (let ((qualifiers '())
        (type-node (treesit-node-child-by-field-name field-decl "type"))
        (current func-decl)
        (suffix ""))

    ;; Collect type qualifiers (const, constexpr, etc.)
    (dolist (child (treesit-node-children field-decl))
      (when (string= (treesit-node-type child) "type_qualifier")
        (push (cpp-func-impl--safe-node-text child t) qualifiers)))

    ;; Walk upward to collect pointer/reference wrappers
    (while (and current (not (eq current field-decl)))
      (let ((node-type (treesit-node-type current)))
        (cond
         ((string= node-type "pointer_declarator")
          (setq suffix (concat "*" suffix)))
         ((string= node-type "reference_declarator")
          (setq suffix (concat "&" suffix)))
         ((string= node-type "rvalue_reference_declarator")
          (setq suffix (concat "&&" suffix)))))
      (setq current (treesit-node-parent current)))

    ;; Final return type assembly (preserves spacing rules for *, &, &&)
    (let ((base (when type-node (cpp-func-impl--safe-node-text type-node t))))
      (string-trim
       (concat
        (when qualifiers
          (concat (string-join (nreverse qualifiers) " ") " "))
        base
        suffix)))))

(defun cpp-func-impl--get-decl-info (node)
  "Return plist of info about the C++ method NODE.

Returns: :class-name, :method-name, :return-type, :text,
optionally :template-param, :qualifiers."
  (let* (;; Find the field_declaration first
         (field-decl
          (treesit-parent-until node
                                (lambda (n)
                                  (member (treesit-node-type n)
                                          '("field_declaration" "declaration")))))

         ;; Find function_declarator within field_declaration
         (func-decl
          (when field-decl
            (treesit-search-subtree field-decl
                                    (lambda (n)
                                      (string= (treesit-node-type n) "function_declarator")))))

         ;; Get surrounding template_declaration (if any)
         (template-decl
          (when field-decl
            (treesit-parent-until field-decl
                                  (lambda (n)
                                    (string= (treesit-node-type n) "template_declaration")))))

         (template-param-list
          (when template-decl
            (treesit-node-child-by-field-name template-decl "parameters")))

         ;; Extract function name
         (name-node
          (when func-decl
            (treesit-search-subtree func-decl
                                    (lambda (n)
                                      (member (treesit-node-type n)
                                              '("identifier" "field_identifier"))))))

         ;; Class name
         (class-node (cpp-func-impl--find-class-node field-decl))
         (class-name
          (when class-node
            (cpp-func-impl--safe-node-text
             (treesit-node-child-by-field-name class-node "name"))))

         (qualifiers
          (when field-decl
            (let ((quals '()))
              (dolist (child (treesit-node-children field-decl))
                (when (string= (treesit-node-type child) "type_qualifier")
                  (push (cpp-func-impl--safe-node-text child t) quals)))
              (nreverse quals))))

         (return-type (cpp-func-impl--extract-return-type field-decl func-decl))

         (template-text
          (when template-param-list
            (cpp-func-impl--safe-node-text template-param-list))))

    (unless (and func-decl name-node class-name)
      (user-error "Could not find method, name, or class context"))

    ;; Return info
    (list :class-name class-name
          :method-name (cpp-func-impl--safe-node-text name-node)
          :qualifiers qualifiers
          :return-type return-type
          :text (cpp-func-impl--safe-node-text func-decl)
          :template-param template-text)))

(defun cpp-func-impl--get-methods-text (nodes)
  "Return display strings for method NODES for selection purposes."
  (let ((display-pairs '()))
    (dolist (node nodes)
      (condition-case err
          (let* ((info (cpp-func-impl--get-decl-info node))
                 (qualified-class-name (cpp-func-impl--get-qualified-class-name node))
                 (ret (plist-get info :return-type))
                 (sig (plist-get info :text))
                 (display (format "%s %s::%s" ret qualified-class-name sig)))
            (push (cons display node) display-pairs))
        (error
         (message "Warning: Skipped malformed method: %s" (error-message-string err)))))
    (nreverse display-pairs)))

;;; Implementation Generation

(defun cpp-func-impl--insert-implementation (node &optional insert-doc)
  "Insert the implementation into buffer.

Takes NODE from `treesit-node-at' at point and optionally INSERT-DOC."
  (let* ((decl (cpp-func-impl--get-decl-info node))
         (class-name (plist-get decl :class-name))
         (method-name (plist-get decl :method-name))
         (text (cpp-func-impl--trim-virtual-specifiers (plist-get decl :text)))
         (return-type (plist-get decl :return-type))
         (template-text (plist-get decl :template-param))
         (qualified-class-name (cpp-func-impl--get-qualified-class-name node))
         (impl (format "%s %s::%s" return-type qualified-class-name text))
         (comment (when insert-doc
                    (cpp-func-impl--format-comment class-name method-name))))

    (when cpp-func-impl-add-newlines
      (insert "\n"))

    (let ((start-pos (point)))
      (when template-text
        (insert (format "template %s\n" template-text)))
      (insert impl "\n{\n")
      (when comment
        (insert "    " comment "\n"))
      (insert "}")

      (when cpp-func-impl-auto-indent
        (indent-region start-pos (point))))

    (when cpp-func-impl-add-newlines
      (insert "\n"))

    (message "Inserted method %s for class %s%s"
             method-name
             class-name
             (if template-text " (template)" ""))))

;;; Interactive Commands

;;;###autoload
(defun cpp-func-impl-implement (&optional insert-doc)
  "Insert a C++ method implementation in the corresponding source file.

This function should be called with point on a C++ method declaration
inside a class definition. It uses Tree-sitter to extract the class
name, method name, return type, and any associated template parameters,
then generates a skeleton implementation in the corresponding .cpp file.

If called with a prefix argument INSERT-DOC (\\[universal-argument]), a
comment placeholder will be inserted inside the function body."
  (interactive "P")
  (cpp-func-impl--ensure-cpp-treesit)
  (condition-case err
      (let ((node (treesit-node-at (point))))
        ;; Jump to the corresponding .cpp file
        (ff-find-other-file)
        ;; Go to end of file to insert implementation
        (goto-char (point-max))
        ;; Insert implementation
        (cpp-func-impl--insert-implementation node insert-doc)
        ;; Move cursor inside the function body
        (search-backward "}")
        (forward-line -1)
        (end-of-line))
        (error
         (user-error "Failed to implement method: %s" (error-message-string err)))))

;;;###autoload
(defun cpp-func-impl-implement-all (&optional insert-doc)
  "Implement all C++ methods of the current class in the corresponding source file.

This function should be called with point inside a C++ class with at least
one method declaration. It generates skeleton implementations for all methods
in the corresponding .cpp file.

If called with a prefix argument INSERT-DOC (\\[universal-argument]), comment
placeholders will be inserted inside function bodies."
  (interactive "P")
  (cpp-func-impl--ensure-cpp-treesit)
  (let ((func-nodes (cpp-func-impl--get-methods)))
    (unless func-nodes
      (user-error "No method declarations found in class"))

    (ff-find-other-file) ;; Jump to .cpp
    (goto-char (point-max)) ;; Append to end of file

    (let ((success-count 0)
          (total-count (length func-nodes)))
      (dolist (node func-nodes)
        (condition-case err
            (cpp-func-impl--insert-implementation node insert-doc)
          (setq success-count (1+ success-count))
          (error
           (message "Skipped method: %s" (error-message-string err)))))

      (message "Implemented %d/%d methods successfully" success-count total-count))))

;;;###autoload
  (defun cpp-func-impl-implement-selected (&optional insert-doc)
    "Implement selected C++ methods of the current class in the corresponding source file.

This function should be called with point inside a C++ class that has at least
one method declaration. It presents a list of methods for selection and generates
skeleton implementations for the chosen methods.

If called with a prefix argument INSERT-DOC (\\[universal-argument]), comment
placeholders will be inserted inside function bodies."
    (interactive "P")
    (cpp-func-impl--ensure-cpp-treesit)
    (let* ((func-nodes (cpp-func-impl--get-methods))
           (node-sigs (cpp-func-impl--get-methods-text func-nodes)))

      (unless node-sigs
        (user-error "No method declarations found in class"))

      (let* (;; Override crm-separator to avoid breaking signatures with commas
             (crm-separator ";")
             (choices (mapcar #'car node-sigs))
             (selected (completing-read-multiple
                        "Select methods (separate with ;): "
                        choices nil t)))

        (unless selected
          (user-error "No methods selected"))

        (ff-find-other-file) ;; Jump to .cpp
        (goto-char (point-max)) ;; Append to end of file

        (let ((success-count 0))
          (dolist (display selected)
            (let ((node (cdr (assoc-string display node-sigs))))
              (if node
                  (condition-case err
                      (cpp-func-impl--insert-implementation node insert-doc)
                    (setq success-count (1+ success-count))
                    (error
                     (message "Failed to implement %s: %s" display (error-message-string err))))
              (message "Warning: Couldn't find node for method: %s" display))))

        (message "Implemented %d/%d selected methods successfully"
                 success-count (length selected))))))

;;;###autoload
(defun cpp-func-impl-concrete-class ()
  "Generate a concrete C++ class implementing all pure virtual methods from the class at point."
  (interactive)
  (cpp-func-impl--ensure-cpp-treesit)
  (let* ((virtual-nodes (cpp-func-impl--get-pure-virtual-methods)))
    (if (not virtual-nodes)
        (message "No pure virtual methods found.")
      (let ((impl-snippets '())
            (base-class-name nil)
            (concrete-class-name (read-string "Enter concrete class name: ")))

        (when (string-empty-p concrete-class-name)
          (user-error "Concrete class name cannot be empty"))

        (dolist (node virtual-nodes)
          (condition-case err
              (let* ((node-info (cpp-func-impl--get-decl-info node))
                     (method-name (plist-get node-info :method-name))
                     (func-text (cpp-func-impl--trim-virtual-specifiers
                                 (plist-get node-info :text)))
                     (return-type (plist-get node-info :return-type))
                     (template-text (plist-get node-info :template-param)))
                (unless base-class-name
                  (setq base-class-name (plist-get node-info :class-name)))
                (push
                 (concat
                  (when template-text
                    (format "    template %s\n" template-text))
                  (format "    %s %s override;"
                          return-type func-text))
                 impl-snippets))
            (error
             (message "Skipped method: %s" (error-message-string err)))))

        ;; Insert the generated class definition
        (goto-char (point-max))
        (let ((beg (point)))
          (insert (format "\nclass %s : public %s {\npublic:\n"
                          concrete-class-name base-class-name))
          (insert (string-join (nreverse impl-snippets) "\n"))
          (insert "\n};\n")
          (when cpp-func-impl-auto-indent
            (indent-region beg (point))))

        (message "Concrete class '%s' created from base '%s' with %d methods."
                 concrete-class-name base-class-name (length impl-snippets))))))

(provide 'cpp-func-impl)

;;; cpp-func-impl.el ends here
