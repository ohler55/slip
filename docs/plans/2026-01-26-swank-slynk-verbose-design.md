# Swank/Slynk Verbose Mode Design

## Overview

Add verbose/debug logging to swank and slynk packages with category-based filtering and optional ANSI colors.

## API

```lisp
;; Enable specific categories with colors
(swank-verbose :wire t :dispatch t :eval t :color t)

;; Query current state
(swank-verbose) => (:wire t :dispatch t :eval nil :color nil)

;; Disable all
(swank-verbose :wire nil :dispatch nil :eval nil)
```

Same API for `slynk-verbose`.

## Categories

| Category | Description |
|----------|-------------|
| `:wire` | Raw S-expressions sent/received over the socket |
| `:dispatch` | Handler name and arguments for each RPC call |
| `:eval` | SLIP expressions being evaluated and results |

## Output Format

```
[swank:wire] <- (:emacs-rex (swank:connection-info) "cl-user" t 1)
[swank:dispatch] connection-info []
[swank:eval] (+ 1 2) => 3
[swank:wire] -> (:return (:ok ...) 1)
```

## Color Scheme (black background)

| Category | Color | ANSI Code |
|----------|-------|-----------|
| `:wire` | Cyan | `\x1b[36m` |
| `:dispatch` | Yellow | `\x1b[33m` |
| `:eval` | Green | `\x1b[32m` |
| Errors | Red | `\x1b[31m` |

## Implementation

- Package-level variables for each flag
- `swank-verbose` / `slynk-verbose` LISP functions to toggle at runtime
- Helper functions for logging each category
- Output goes to stdout
