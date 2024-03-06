# How to contribute to Plume

This file is a guide to guide you through the process of contributing to Plume. It is a set of the conventions and rules that we follow to maintain the project.

### Haskell file header

To correctly maintain the project, we're using a file header that is the quite same as the one described by the Haskell programming guidelines.

```hs
{-
Module      :  <File name or $Header$ to be replaced automatically>
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen

<module description starting at first column>
-}
```

For more information, you can check the [Haskell programming guidelines](https://wiki.haskell.org/Programming_guidelines).

### Code style

We're following the [Haskell programming guidelines](https://wiki.haskell.org/Programming_guidelines) to write our code. This includes the following:

- Indentation: 2 spaces
- Line length: 80 characters
- Naming conventions: camelCase for variables and functions, PascalCase for types and data constructors.
- Imports: one import per line, and the imports should be grouped by the source of the module.
- Language extensions: should be placed at the top of the file, after the file header and before the module declaration.
- Formatter: We're using Fourmolu to format our code. You can install it by running `cabal install fourmolu` and then run it with `fourmolu <file>`.

### Commit messages

We're following the [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) specification to write our commit messages. This convention makes it easier to understand the changes that are made in the repository.

So, the commit message should be structured as follows:

```
<type>(optional scope): <description>
```

Where `type` is one of the following:

- `build`: Changes that affect the build system or external dependencies
- `ci`: Changes to our CI configuration files and scripts
- `docs`: Changes that don't modify src or test files
- `feat`: A new feature that can be:
  - A new compiler or language feature introduction
  - Some updates to the language specification
- `fix`: A bug fix that can be:
  - A bug fix in the compiler
  - A bug fix in the language specification
  - Also a typo fix in the documentation or in the code
  - A fix to the compiler or language specification
- `perf`: A code change that improves performance
- `refactor`: A code change that neither fixes a bug nor adds a feature, often a code cleanup or a name change
- `test`: Adding missing tests or correcting existing tests

The `scope` is optional and can be anything specifying the place of the commit change. For example, `parser`, `lexer`, `docs`, `readme`, etc.

The `description` should be a short description of the change. It should be written in the imperative mood and should not end with a period.

### Branch naming

Branch naming isn't strict, all we ask for is to use a comprehensive name that describes the changes that are made in the branch. For example, `feat/lexer-exceptions`, `fix/parser-priority`, `docs/readme-sample`, etc.
