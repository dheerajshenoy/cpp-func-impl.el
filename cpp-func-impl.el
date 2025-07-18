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

(defun cpp-func-impl--get-decl-info ()
  "Return plist of info about the C++ method at point, supporting template and regular methods.
Returns: `:class-name`, `:method-name`, `:return-type`, `:text`, optionally `:template-param`."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         ;; Step 1: Get function_declarator
         (func-decl
          (treesit-parent-until node
                                (lambda (n)
                                  (string= (treesit-node-type n) "function_declarator")))))

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


;;;###autoload
(defun cpp-func-impl-implement (&optional insert-doc)
  "Insert a C++ method implementation in the corresponding source file.

When point is on a method declaration inside a class (including
templated methods), this function uses Tree-sitter to extract relevant
information such as the class name, method name, return type, and
template parameters. It then inserts a properly scoped function
definition at the end of the corresponding .cpp file.

If called with a prefix argument INSERT-DOC (i.e. via \\[universal-argument]),
a placeholder comment is added inside the function body to remind the user
to implement it.

NOTE: This function supports both regular and templated methods and requires
Tree-sitter support for C++ to be enabled."
  (interactive "P")
  (let* ((info (cpp-func-impl--get-decl-info))
         (class-name (plist-get info :class-name))
         (method-name (plist-get info :method-name))
         (text (plist-get info :text))
         (return-type (plist-get info :return-type))
         (template-text (plist-get info :template-param))
         (impl (format "%s %s::%s"
                       return-type class-name text)))

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
          (insert (format "// TODO: Implement `%s`" method-name))
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
