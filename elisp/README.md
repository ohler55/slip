# Emacs Lisp Tests for SLIP Swank/Slynk Servers

This directory contains Emacs Lisp files for interactively testing the SLIP Swank and Slynk servers.

## Files

| File | Description | Requirements |
|------|-------------|--------------|
| `test-swank.el` | Tests for SLIME integration | SLIME installed |
| `test-slynk.el` | Tests for Sly integration | Sly installed |
| `test-swank-raw.el` | Raw socket tests | No dependencies |

## Quick Start

### Option 1: Raw Socket Tests (No Dependencies)

This is the easiest way to test - no SLIME or Sly required.

1. Start the SLIP server:
   ```bash
   # For swank (SLIME protocol)
   slip -e '(swank:swank-server :port 4005)'

   # Or for slynk (Sly protocol)
   slip -e '(slynk:slynk-server :port 4005)'
   ```

2. In Emacs:
   ```
   M-x load-file RET /path/to/elisp/test-swank-raw.el RET
   M-x slip-raw-test-all RET
   ```

### Option 2: SLIME Integration Tests

Requires [SLIME](https://github.com/slime/slime) installed.

1. Start the SLIP swank server:
   ```bash
   slip -e '(swank:swank-server :port 4005)'
   ```

2. In Emacs:
   ```
   M-x load-file RET /path/to/elisp/test-swank.el RET
   M-x slip-swank-connect RET
   M-x slip-swank-test-all RET
   ```

### Option 3: Sly Integration Tests

Requires [Sly](https://github.com/joaotavora/sly) installed.

1. Start the SLIP slynk server:
   ```bash
   slip -e '(slynk:slynk-server :port 4005)'
   ```

2. In Emacs:
   ```
   M-x load-file RET /path/to/elisp/test-slynk.el RET
   M-x slip-slynk-connect RET
   M-x slip-slynk-test-all RET
   ```

## Available Commands

### test-swank.el / test-slynk.el

| Command | Description |
|---------|-------------|
| `slip-swank-connect` / `slip-slynk-connect` | Connect to server |
| `slip-swank-disconnect` / `slip-slynk-disconnect` | Disconnect |
| `slip-swank-test-all` / `slip-slynk-test-all` | Run all tests |
| `slip-swank-eval` / `slip-slynk-eval` | Evaluate expression |
| `slip-swank-describe` / `slip-slynk-describe` | Describe symbol |
| `slip-swank-complete` / `slip-slynk-complete` | Complete prefix |

### test-swank-raw.el

| Command | Description |
|---------|-------------|
| `slip-raw-connect` | Connect to server |
| `slip-raw-disconnect` | Disconnect |
| `slip-raw-test-all` | Run all tests |
| `slip-raw-eval` | Evaluate expression |

## Individual Tests

You can run individual tests interactively:

```elisp
;; Connection
M-x slip-swank-test-connection-info

;; Evaluation
M-x slip-swank-test-eval-simple
M-x slip-swank-test-eval-string
M-x slip-swank-test-eval-defun

;; Completions
M-x slip-swank-test-completions
M-x slip-swank-test-fuzzy-completions
M-x slip-swank-test-apropos

;; Documentation
M-x slip-swank-test-describe-symbol
M-x slip-swank-test-documentation
M-x slip-swank-test-arglist

;; Packages
M-x slip-swank-test-list-packages
M-x slip-swank-test-set-package
```

## Configuration

Change the host/port in the elisp files if needed:

```elisp
(setq slip-swank-host "127.0.0.1")
(setq slip-swank-port 4005)
```

Or for raw socket tests:

```elisp
(setq slip-raw-host "127.0.0.1")
(setq slip-raw-port 4005)
```

## Test Results

After running tests, results are displayed in a temporary buffer showing pass/fail status for each test.
