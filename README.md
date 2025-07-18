# cpp-func-impl.el

C++ class methods implementation timesaver.

- [Overview](#overview)
- [Demo](#demo)
- [Features](#features)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
- [Customizations](#customizations)
- [`cpp-func-impl-comment-string`](#cpp-func-impl-comment-string)
- [Limitations](#limitations)
- [License](#license)
- [Contributing](#contributing)

# Overview

This Emacs package provides a utility to generate out-of-line method implementations for C++ classes. It leverages [Tree Sitter](https:tree-sitter.github.io/tree-sitter/) for accurate parsing of C++ declarations directly from header files.

Place your cursor on a C++ method declaration inside a header file, run the command, and the function body will be automatically generated and inserted in the corresponding source (.cpp) file.

Templated methods are fully supported.

> [!CAUTION]
> I am new to Emacs Lisp and this plugin is still in early stages and it might not work all the time.
> Please open an issue if there are any problems.

# Demo

1. Implement function at point `cpp-func-impl-implement`

-   Regular functions

![img](images/regular.gif)

-   Templated functions

![img](images/templated.gif)

2. Implement all methods from a class `cpp-func-impl-implement-all`

![img](images/implement-all.gif)

3. Implement selected methods from a class `cpp-func-impl-implement-selected`

![img](images/implement-selected.gif)

4. Create concrete class from another class `cpp-func-impl-concrete-class`

![img](images/concrete-class.gif)

# Features

+ Implement all the methods in a class
+ Implement function at point
+ Implement concrete class from a given class

{> [!NOTE]
> Concrete class are ordinary classes that can be instantiated and have no virtual functions.

# Requirements

-   Emacs 29+ with Tree-sitter support.
-   C++ major mode using Tree-sitter backend (\`c++-ts-mode\`).
-   A valid project structure with corresponding \`.cpp\` files discoverable via \`ff-find-other-file\`.

# Installation

1. Clone this repo and add it to load path and add the following to your ``init.el`` file:

```elisp
(require 'cpp-func-impl)
```

# Usage

You have the following functions that can be invoked:

+ `cpp-func-impl-implement` - Implements the method at point.
+ `cpp-func-impl-implement-selected` - Implements selected methods from a class.
+ `cpp-func-impl-implement-all` - Implements all the method inside the class (assuming the point is inside the class).
+ `cpp-func-impl-concrete-class` - Creats a concrete class of the class where the point lies inside.


If you call the commands with a prefix argument a comment is inserted.
This comment can be changed by setting the variable
`cpp-func-impl-comment-string`.

# Customizations

You can customize the comment inserted into the implementation body by setting the variable `cpp-func-impl-comment-string`.

## `cpp-func-impl-comment-string`

The comment string can include valid format specifiers mentioned below which get injected with information like `method-name`, `class-name`, `time`, `date`.

Following format specifiers are supported:

| Format Specifier | Meaning      |
|------------------|--------------|
| %c               | Class Name   |
| %m               | Method Name  |
| %t               | Current Time |
| %d               | Current Date |

```elisp
(use-package cpp-func-impl
  :ensure nil
  :load-path "<load-path>"
  :custom
  (cpp-func-impl-comment-string "// TODO: Implement `%m` for the class `%c. Added at `%t` on `%d`"))
```

# Limitations

-   Requires header/source pair to be correctly mapped.
-   Does not support auto-discovery of already existing method definition.

# TODO

- [x] Nested class environments
- [ ] Auto discovery of already existing method definition.


# License

MIT License

# Contributing

Contributions, suggestions, or bug reports are welcome. Feel free to fork and submit a pull request or open an issue.
