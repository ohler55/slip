# Feature Slime Outstanding TODOs

This list is limited to work that is still clearly outstanding on `feature/slime`.
It excludes generic repository `TODO` markers that are unrelated to SLIME/Swank.

## Done

- [x] Implement `:emacs-interrupt` handling — cancels active evaluation, returns `:abort`.
- [x] Interrupt test coverage — tests mid-eval interrupt and no-active-eval cases.
- [x] Add `ping`, `compile-string-for-emacs`, `compile-file-for-emacs`, `load-file`, `update-indentation-information`, `inspector-call-nth-action` handlers.
- [x] Silence noisy swank test output via configurable `LogOutput` writer.

## Priority 1

- [ ] Run end-to-end SLIME validation against a real Emacs client.
  Minimum pass: connect, evaluate, compile (C-c C-c, C-c C-k), inspect, xref, trace, interrupt.

- [ ] Reconcile `feature/slime` with `origin/master` before merge.
  As of 2026-03-23, the branch is behind `origin/master` by 2 commits.

## Priority 2

- [ ] Add type/class detection to completion flags in `pkg/swank/rpc_complete.go:121`.
  Definition of done: completion metadata sets the relevant flag bits for classes/types when that information is available.

- [ ] Fix `macroexpandFull` — macros panic during expansion in test context. Either fix the underlying macro expansion or mark as known limitation.

## Priority 3 (Low — nice-to-have for fuller SLIME compatibility)

- [ ] `pprint-inspector-part` — pretty-print in inspector context.
- [ ] `inspector-eval` — evaluate in inspector context.
- [ ] `inspector-history` — inspector navigation history display.
- [ ] `inspector-toggle-verbose` — toggle verbose inspector mode.
- [ ] `describe-inspectee` — describe current inspector object.
- [ ] `complete-form` — form template completion (Tab after opening paren).
- [ ] `completions-for-character` — `#\` character name completion.
- [ ] `fuzzy-completion-selected` — notify on fuzzy completion selection (stub OK).
- [ ] Push swank test coverage past 94% — requires refactoring `getFeatures` for testability and fixing macro expansion in test scope.
