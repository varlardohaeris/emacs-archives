This package provides a utility for deducing the repository root directory
for a given file, based on pre-defined or user provided matching criteria.

It is specifically designed to be useful in a heterogeneous environment,
where the developer is using several source control tools and types of
projects.

Once the repository root directory is deduced, it is cached (per buffer) for
future reference in subsequent calls to repository-root.

In itself this library isn't that useful - it's meant to be used by
other libraries that may benefit from knowing where the repository root
directory resides (e.g. for limiting search effort).

Nested / embedded repositories are also supported, but this feature
is disabled by default. Please refer to the documentation of the
customization variable `repository-root-exhaustive-scan'.

Installation:

1. Put this file in a directory that is a member of load-path, and
   byte-compile it (e.g. with `M-x byte-compile-file') for better
   performance.
2. Add the following to your ~/.emacs:
   (require 'repository-root)

Usage:

1. add repository root matching criterions to the list repository-root-matchers
   either directly:
      (add-to-list 'repository-root-matchers repository-root-matcher/git)
   or via the repository-root customization group.

   you should order the matchers in increasing order of matching likelihood:
   the matcher most likely to match should be first on the list.

2. use the function (repository-root path) to find the repository root directory
   corresponding to the given path string - this is the first parent directory
   that is matched by at least one criterion in repository-root-matchers.

Currently, the following repository root matching criterions are supported:

1. Pre-defined (built-in) matchers (e.g. repository-root-matcher/git).

2. Rules: a matching rule together with path string snippet. The default
   rule is that the path string has to match a file or directory in the repository
   root directory, but not in any of the repository's sub-directory.

3. Functions: with two input strings (parent-path-to-test path),
   that returns non-nil if parent-path-to-test is the repository root directory
   corresponding to the given path string.
   If the return value is a string, it is treated as the end-result root directory.
   This may be useful if you have your own directory search method.
