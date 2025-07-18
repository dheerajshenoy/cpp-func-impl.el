;;; cpp-func-impl.el --- C++ methods implement timesaver -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Maintainer: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29") (treesitter))
;; Keywords: convenience, c++, c++-ts
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
;;
;; When invoked (`cpp-func-impl-implement`), the function:
;;   1. Parses the current buffer to find the method declaration at point.
;;   2. Extracts structural information using Tree-sitter nodes.
;;   3. Switches to the corresponding implementation (.cpp) file using `ff-find-other-file`.
;;   4. Inserts the appropriate function skeleton at the end of the file, including:
;;        - A `template` line if the method is templated
;;        - The fully qualified method name
;;        - An empty function body
;;
;; This is especially useful in large C++ projects to avoid repetitive typing.
;;
;; Requirements:
;; - Emacs 29+ with Tree-sitter enabled
;; - A properly loaded C++ Tree-sitter grammar
;;
;; Usage: Place cursor on a method declaration inside a class, and
;;   run: M-x cpp-func-impl-implement or bind it to a key for convenience.


;;; Group

(defgroup cpp-func-impl nil
  "Inserts C++ method implementations from class declarations using Tree-Sitter."
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

;;; Helper functions

(defun cpp-func-impl--insert-implementation (template-text implementation comment &optional insert-doc)
  "Inserts the implementation to the buffer given TEMPLATE-TEXT,
IMPLEMENTATION, COMMENT and optionally INSERT-DOC."
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


(defun cpp-func-impl--get-methods ()
  "Returns a list of all method nodes in the class at point using Tree-sitter."
  (interactive)
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
        (let* ((func-decl (treesit-search-subtree
                           decl
                           (lambda (n)
                             (string= (treesit-node-type n) "function_declarator")))))
          (when func-decl
            (push func-decl methods))))
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
          (let ((virtual-node
                 (treesit-filter-child child
                                         (lambda (n)
                                           (string= (treesit-node-type n) "virtual")))))
            (when virtual-node
              ;; Get the function_declarator inside this declaration
              (let ((func-decl
                     (treesit-search-subtree child
                                             (lambda (n)
                                               (string= (treesit-node-type n) "function_declarator")))))
                (when func-decl
                  (push func-decl method-nodes)))))))
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
              (let ((func-decl
                     (treesit-search-subtree child
                                             (lambda (n)
                                               (string= (treesit-node-type n) "function_declarator")))))
                (when func-decl
                  (push func-decl method-nodes)))))))
      (nreverse method-nodes))))


(defun cpp-func-impl--get-decl-info (node)
  "Return plist of info about the C++ method of `node`, supporting template and regular methods.
Returns: `:class-name`, `:method-name`, `:return-type`, `:text`, optionally `:template-param`."
  (let* (;; Step 1: Get function_declarator
         (func-decl
          (if (string= (treesit-node-type node) "function_declarator")
              node
            (treesit-parent-until node
                                  (lambda (n)
                                    (string= (treesit-node-type n) "function_declarator"))))))

    (unless func-decl
      (user-error "No function declarator found at point"))

    ;; Step 2: Get declaration (wrapped in template or not)
    (let* ((declaration
            (treesit-parent-until func-decl
                                  (lambda (n)
                                    (member (treesit-node-type n)
                                            '("declaration" "field_declaration")))))
           ;; Get surrounding template_declaration (if any)
           (template-decl
            (treesit-parent-until declaration
                                  (lambda (n)
                                    (string= (treesit-node-type n) "template_declaration"))))
           ;; Now extract the parameter list node
           (template-param-list
            (when template-decl
              (treesit-node-child-by-field-name template-decl "parameters")))
           ;; Step 3: Extract function name manually
           (name-node
            (treesit-search-subtree func-decl
                                    (lambda (n)
                                      (member (treesit-node-type n)
                                              '("identifier" "field_identifier")))))
           ;; Step 4: Return type
           (type-node (treesit-node-child-by-field-name declaration "type"))
           ;; Step 5: Class name
           (class-node
            (treesit-parent-until declaration
                                  (lambda (n)
                                    (string= (treesit-node-type n) "class_specifier"))))
           (class-name
            (when class-node
              (treesit-node-text
               (treesit-node-child-by-field-name class-node "name"))))
           (template-text (when template-param-list
                            (treesit-node-text template-param-list))))

      (unless (and name-node class-name)
        (user-error "Could not find method name or class name"))

      ;; Step 6: Return info
      (list :class-name class-name
            :method-name (treesit-node-text name-node)
            :return-type (when type-node
                           (treesit-node-text type-node))
            :text (treesit-node-text func-decl)
            :template-param template-text))))


(defun cpp-func-impl--format-comment (class-name method-name)
  "Format the comment string using the different format specifiers.

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
         (impl (format "%s %s::%s"
                       return-type class-name text))
         (comment (cpp-func-impl--format-comment class-name method-name)))


    ;; Jump to the corresponding .cpp file
    (call-interactively 'ff-find-other-file)
    ;; Go to end of file to insert implementation
    (goto-char (point-max))

    ;; Insert implementation
    (insert "\n")
    (when template-text
      (insert (format "template %s" template-text) "\n"))
    (insert impl "\n{\n")
    (if insert-doc
        (progn
          (insert comment)
          (indent-region (line-beginning-position) (line-end-position))
          (insert "\n"))
      (insert "\n"))
    (insert "}\n")

    ;; Move cursor inside the function body
    (forward-line -2)

    (message "Inserted method %s for class %s%s"
             method-name
             class-name
             (if template-text " (template)" ""))))


(provide 'cpp-func-impl)
