;;; cpp-func-impl.el --- C++ methods implementation timesaver -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Maintainer: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, c++, treesitter
;; URL: https://github.com/dheerajshenoy/cpp-func-impl.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a utility for generating out-of-class C++ method
;; implementations directly from header declarations. It uses Emacs's built-in
;; Tree-sitter support to parse C++ syntax trees, allowing it to accurately extract
;; the syntax information of the method.

;; The available commands are:
;; 1. `cpp-func-impl-implement'
;; 2. `cpp-func-impl-implement-all'
;; 3. `cpp-func-impl-concrete-class'
;;
;; When one of the implement class of functions are called, the following steps are followed:
;;
;;   1. Parses the current buffer to find the method declaration at point.
;;   2. Extracts structural information using Tree-sitter nodes.
;;   3. Switches to the corresponding implementation (.cpp) file using `ff-find-other-file`.
;;   4. Inserts the appropriate function skeleton at the end of the file, including:
;;        - A `template` line if the method is templated
;;        - The fully qualified method name
;;        - An empty function body
;;
;; NOTE: In the case of `cpp-func-impl-implement-all' all of the methods in the class are
;; implemented
;;
;; This is especially useful in large C++ projects to avoid repetitive typing.
;;
;; Requirements:
;; - Emacs 29.1+ with Tree-sitter enabled
;; - A properly loaded C++ Tree-sitter grammar
;;
;; Usage: Place cursor on a method declaration inside a class,
;;   run: M-x cpp-func-impl-implement to implement the function at point
;;   run: M-x cpp-func-impl-implement-all with point inside the class to
;;        implement all the methods inside the class
;;   run: M-x cpp-func-impl-concrete-class to inherit from given class
;;        and implement all the virtual methods

;;; Code:

;;;; Group

(defgroup cpp-func-impl nil
  "Inserts C++ method implementations from class declarations using treesitter."
  :group 'tools
  :prefix "cpp-func-impl-")

;;; Custom Variables

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

;;;; Helper functions

(defun cpp-func-impl--get-methods-text (nodes)
  "Returns all the methods' name. Useful for selecting methods.

Argument is the list of NODES for which the names are to be returned."
  (let ((display-pairs '()))
    (dolist (node nodes)
      (let* ((info (cpp-func-impl--get-decl-info node))
             (qualified-class-name (cpp-func-impl--get-qualified-class-name node))
             (ret (plist-get info :return-type))
             (sig (plist-get info :text))
             (display (format "%s %s::%s" ret qualified-class-name sig)))
        (push (cons display node) display-pairs)))
    display-pairs))

(defun cpp-func-impl--insert-implementation (template-text implementation comment &optional insert-doc)
  "Inserts the implementation to buffer.

This function takes in TEMPLATE-TEXT, IMPLEMENTATION, COMMENT and
optionally INSERT-DOC."
  (insert "\n")
  (when template-text
    (insert (format "template %s" template-text) "\n"))
  (insert implementation "\n{\n")
  (if insert-doc
      (progn
        (insert comment)
        (indent-region (line-beginning-position) (line-end-position))
        (insert "\n"))
    (insert "\n"))
  (insert "}\n"))

(defun cpp-func-impl--get-qualified-class-name (node)
  "Walks up the AST from NODE to collect nested class names."
  (let ((names '()))
    (while node
      (when (string= (treesit-node-type node) "class_specifier")
        (when-let* ((name-node (treesit-node-child-by-field-name node "name")))
            (push (treesit-node-text name-node) names)))
      (setq node (treesit-node-parent node)))
    (string-join names "::")))

(defun cpp-func-impl--get-methods ()
  "Returns a list of all method nodes in the class at point using Tree-sitter."
  (let* ((node (treesit-node-at (point)))
         (class-node (treesit-parent-until node
                                           (lambda (n)
                                             (string= (treesit-node-type n) "class_specifier")))))
    (unless class-node
      (user-error "Not inside a class declaration"))

    (let* ((body (treesit-node-child-by-field-name class-node "body"))
           (decls (treesit-filter-child
                   body
                   (lambda (n)
                     (member (treesit-node-type n) '("field_declaration" "template_declaration")))
                   t))
           (methods '()))
      (dolist (decl decls)
        (when-let* ((func-decl (treesit-search-subtree
                                decl
                                (lambda (n)
                                  (string= (treesit-node-type n) "function_declarator")))))
          (push func-decl methods)))
      (nreverse methods))))

(defun cpp-func-impl--get-virtual-methods ()
  "Return a list of all virtual method declarator nodes in the class at point using Tree-sitter."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (class-node (treesit-parent-until node
                                           (lambda (n)
                                             (string= (treesit-node-type n) "class_specifier")))))
    (unless class-node
      (user-error "Not inside a class declaration"))

    (let* ((body (treesit-node-child-by-field-name class-node "body"))
           (method-nodes '()))
      (dolist (child (treesit-filter-child body #'identity t))
        ;; Look for declarations and template_declarations
        (when (member (treesit-node-type child) '("field_declaration" "template_declaration"))
          (when-let* ((virtual-node
                       (treesit-filter-child child
                                             (lambda (n)
                                               (string= (treesit-node-type n) "virtual")))))
            ;; Get the function_declarator inside this declaration
            (when-let* ((func-decl
                         (treesit-search-subtree child
                                                 (lambda (n)
                                                   (string= (treesit-node-type n) "function_declarator")))))
              (push func-decl method-nodes)))))
      (nreverse method-nodes))))

(defun cpp-func-impl--get-pure-virtual-methods ()
  "Return a list of all *pure* virtual method declarator nodes in the class at point using Tree-sitter."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (class-node (treesit-parent-until node
                                           (lambda (n)
                                             (string= (treesit-node-type n) "class_specifier")))))
    (unless class-node
      (user-error "Not inside a class declaration"))

    (let* ((body (treesit-node-child-by-field-name class-node "body"))
           (method-nodes '()))
      (dolist (child (treesit-filter-child body #'identity t))
        (when (member (treesit-node-type child) '("field_declaration" "template_declaration"))
          (let ((has-virtual
                 (treesit-filter-child child
                                       (lambda (n)
                                         (string= (treesit-node-type n) "virtual"))))
                (has-eq-zero
                 (treesit-filter-child child
                                       (lambda (n)
                                         (and (string= (treesit-node-type n) "number_literal")
                                              (string= (treesit-node-text n) "0"))))))
            (when (and has-virtual has-eq-zero)
              (message (treesit-node-text child))
              (when-let* ((func-decl
                           (treesit-search-subtree child
                                                   (lambda (n)
                                                     (string= (treesit-node-type n) "function_declarator")))))
                (push func-decl method-nodes))))))
      (nreverse method-nodes))))

(defun cpp-func-impl--get-decl-info (node)
  "Return plist of info about the C++ method of `NODE`, supporting template and regular methods.
Returns: `:class-name`, `:method-name`, `:return-type`, `:text`, optionally `:template-param`."
  (let* (;; Always find the field_declaration first
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

         ;; Extract return type
         (type-node
          (when field-decl
            (treesit-node-child-by-field-name field-decl "type")))

         ;; Class name
         (class-node
          (when field-decl
            (treesit-parent-until field-decl
                                  (lambda (n)
                                    (string= (treesit-node-type n) "class_specifier")))))

         (class-name
          (when class-node
            (treesit-node-text
             (treesit-node-child-by-field-name class-node "name"))))

         (template-text
          (when template-param-list
            (treesit-node-text template-param-list))))

    (unless (and func-decl name-node class-name)
      (user-error "Could not find method, name, or class context"))

    ;; Return info
    (list :class-name class-name
          :method-name (treesit-node-text name-node)
          :return-type (when type-node
                         (treesit-node-text type-node))
          :text (treesit-node-text func-decl)
          :template-param template-text)))

(defun cpp-func-impl--format-comment (class-name method-name)
  "Format the comment string using the different format specifiers.

This function takes in CLASS-NAME and METHOD-NAME.

Valid format specifiers are:

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
         ("%t" time)))
     cpp-func-impl-comment-string)))

;;;; Interactive functions

;;;###autoload
(defun cpp-func-impl-implement (&optional insert-doc)
  "Insert a C++ method implementation in the corresponding source file.

This function should be called with point on a C++ method declaration
inside a class definition. It uses Tree-sitter to extract the class
name, method name, return type, and any associated template parameters,
then generates a skeleton implementation in the corresponding .cpp file.

The implementation is appended at the end of the .cpp file, with correct
namespace qualification and template declarations (if applicable).

If called with a prefix argument INSERT-DOC (\\[universal-argument]), a
comment placeholder will be inserted inside the function body. The
comment text can be customized via the `cpp-func-impl-default-comment`
variable.

Note: Tree-sitter support for C++ must be enabled in the current buffer
for this command to work."
  (interactive "P")
  (let* ((info (cpp-func-impl--get-decl-info (treesit-node-at (point))))
         (class-name (plist-get info :class-name))
         (method-name (plist-get info :method-name))
         (text (plist-get info :text))
         (return-type (plist-get info :return-type))
         (template-text (plist-get info :template-param))
         (qualified-class-name (cpp-func-impl--get-qualified-class-name (treesit-node-at (point))))
         (impl (format "%s %s::%s"
                       return-type qualified-class-name text))
         (comment (cpp-func-impl--format-comment class-name method-name)))

    ;; Jump to the corresponding .cpp file
    (ff-find-other-file)

    ;; Go to end of file to insert implementation
    (goto-char (point-max))

    ;; Insert implementation
    (cpp-func-impl--insert-implementation template-text impl comment)

    ;; Move cursor inside the function body
    (forward-line -2)

    (message "Inserted method %s for class %s%s"
             method-name
             class-name
             (if template-text " (template)" ""))))

;;;###autoload
(defun cpp-func-impl-concrete-class ()
  "Generate a concrete C++ class implementing all pure virtual methods from the class at point."
  (interactive)
  (let* ((virtual-nodes (cpp-func-impl--get-pure-virtual-methods)))
    (if (not virtual-nodes)
        (message "No pure virtual methods found.")
      (let ((impl-snippets '())
            (base-class-name nil)
            (concrete-class-name (read-string "Enter a concrete class name: ")))
        (dolist (node virtual-nodes)
          (condition-case err
              (let* ((node-info (cpp-func-impl--get-decl-info node))
                     (method-name (plist-get node-info :method-name))
                     (func-text (plist-get node-info :text))
                     (return-value (plist-get node-info :return-type))
                     (template-text (plist-get node-info :template-param)))
                (unless base-class-name
                  (setq base-class-name (plist-get node-info :class-name)))
                (push
                 (concat
                  (when template-text
                    (format "template %s\n" template-text))
                  (format "%s %s override;"
                          return-value func-text method-name))
                 impl-snippets))
            (error
             (message "Skipped method: %s" (error-message-string err)))))

        ;; Insert the generated class definition
        (goto-char (point-max))
        (let ((beg (point)))
          (insert (format "\nclass %s : public %s\n{\npublic:\n"
                          concrete-class-name base-class-name))
          (insert (string-join (nreverse impl-snippets) "\n"))
          (insert "\n};\n")
          (indent-region beg (point)))
        (message "Concrete class '%s' created from base '%s'."
                 concrete-class-name base-class-name)))))

;;;###autoload
(defun cpp-func-impl-implement-all (&optional insert-doc)
  "Implements all the C++ methds of given class in the corresponding source file.

This function should be called with point inside C++ class with atleast
one method declaration inside. It uses Tree-sitter to extract the class
name, method name, return type, and any associated template parameters,
then generates a skeleton implementation in the corresponding .cpp file.

The implementation is appended at the end of the .cpp file, with correct
namespace qualification and template declarations (if applicable).


If called with a prefix argument INSERT-DOC (\\[universal-argument]), a
comment placeholder will be inserted inside the function body. The
comment text can be customized via the `cpp-func-impl-default-comment`
variable.

NOTE: Nested namespace and class may not work.

Note: Tree-sitter support for C++ must be enabled in the current buffer
for this command to work.

If called with a prefix (\\[universal-argument]]) (INSERT-DOC), a
comment is added in the body of the function implementation stub."
  (interactive)
  (let* ((func-nodes (cpp-func-impl--get-methods)))

    ;; Collect all implementations
    (let ((impl-snippets '()))
      (when func-nodes
        (dolist (node func-nodes)
          (condition-case err
              (let* ((node-info (cpp-func-impl--get-decl-info node))
                     (method-name (plist-get node-info :method-name))
                     (class-name (plist-get node-info :class-name))
                     (func-text (plist-get node-info :text))
                     (return-value (plist-get node-info :return-type))
                     (template-text (plist-get node-info :template-param))
                     (qualified-class-name (cpp-func-impl--get-qualified-class-name node))
                     (comment (cpp-func-impl--format-comment class-name method-name))
                     (impl (concat
                            (when template-text
                              (format "template %s\n" template-text))
                            (format "%s %s::%s\n{\n" return-value qualified-class-name func-text)
                            (if insert-doc
                                (concat comment "\n")
                              "")
                            "}\n")))
                (push impl impl-snippets))
            (error
             (message "Skipped method: %s" (error-message-string err))))))

      ;; Insert all at once
      (ff-find-other-file)
      (goto-char (point-max))
      (message "Inserted %d method implementations." (length impl-snippets))
      (insert "\n" (string-join (nreverse impl-snippets) "\n") "\n"))))

;;;###autoload
(defun cpp-func-impl-implement-selected (&optional insert-doc)
  "Implements selected C++ methds of given class in the corresponding source file.

This function should be called with point inside C++ class with atleast
one method declaration. It uses Tree-sitter to extract the class
name, method name, return type, and any associated template parameters,
then generates a skeleton implementation in the corresponding .cpp file.

The implementation is appended at the end of the .cpp file, with correct
namespace qualification and template declarations (if applicable).

If called with a prefix argument INSERT-DOC (\\[universal-argument]), a
comment placeholder will be inserted inside the function body. The
comment text can be customized via the `cpp-func-impl-default-comment`
variable.

NOTE: Nested namespace and class may not work.

Note: Tree-sitter support for C++ must be enabled in the current buffer
for this command to work.

If called with a prefix (\\[universal-argument]]) (INSERT-DOC), a
comment is added in the body of the function implementation stub."
  (interactive)
  (let* ((func-nodes (cpp-func-impl--get-methods))
         (node-sigs (cpp-func-impl--get-methods-text func-nodes))
         (choices (mapcar #'car node-sigs))
         (selected (completing-read-multiple "Select methods to implement: "
                                             choices nil t))
         (selected-nodes (mapcar (lambda (disp)
                                   (cdr (assoc disp node-sigs)))
                                 selected))
         (impl-snippets '()))

    ;; Collect all implementations
    (dolist (node selected-nodes)
      (condition-case err
          (let* ((node-info (cpp-func-impl--get-decl-info node))
                 (method-name (plist-get node-info :method-name))
                 (class-name (plist-get node-info :class-name))
                 (func-text (plist-get node-info :text))
                 (return-value (plist-get node-info :return-type))
                 (template-text (plist-get node-info :template-param))
                 (qualified-class-name (cpp-func-impl--get-qualified-class-name node))
                 (comment (cpp-func-impl--format-comment class-name method-name))
                 (impl (concat
                        (when template-text
                          (format "template %s\n" template-text))
                        (format "%s %s::%s\n{\n" return-value qualified-class-name func-text)
                        (when insert-doc
                            (concat comment "\n"))
                        "}\n")))
            (push impl impl-snippets))
        (error
         (message "Skipped method: %s" (error-message-string err)))))

  ;; Insert all at once
  (ff-find-other-file)
  (goto-char (point-max))
  (message "Inserted %d method implementations." (length impl-snippets))
  (insert "\n" (string-join (nreverse impl-snippets) "\n") "\n")))

(provide 'cpp-func-impl)

;;; cpp-func-impl.el ends here
