# Feature Slime Outstanding TODOs

This list is limited to work that is still clearly outstanding on `feature/slime`.
It excludes generic repository `TODO` markers that are unrelated to SLIME/Swank.

## Priority 1

- [ ] Implement `:emacs-interrupt` handling in [pkg/swank/connection.go](/Users/joe/Dev/slip/slip/.worktrees/feature-slime/pkg/swank/connection.go#L189).
  Current behavior is a stub, and the existing test only asserts that the message does not crash the server in [test/swank/edge_cases_test.go](/Users/joe/Dev/slip/slip/.worktrees/feature-slime/test/swank/edge_cases_test.go#L206).
  Definition of done: interrupt requests produce a deliberate SLIME-compatible outcome instead of being silently ignored.

## Priority 2

- [ ] Decide and implement the intended SLIME semantics for interrupts.
  Open question: should `:emacs-interrupt` cancel the current evaluation, mark the connection state, or return a specific abort/condition payload?
  This should be resolved before refining tests, otherwise the implementation target is underspecified.

- [ ] Expand interrupt test coverage once behavior is defined.
  The current test only covers "does not crash". Add assertions for the actual protocol response and for interruption during an in-flight evaluation if the implementation supports that.

- [ ] Add type/class detection to completion flags in [pkg/swank/rpc_complete.go](/Users/joe/Dev/slip/slip/.worktrees/feature-slime/pkg/swank/rpc_complete.go#L121).
  This is the only other explicit Swank `TODO` in the branch.
  Definition of done: completion metadata sets the relevant flag bits for classes/types when that information is available.

## Priority 3

- [ ] Run end-to-end SLIME validation against a real Emacs client.
  The Swank tests rely on real TCP connections, and this workspace sandbox cannot bind test listeners, so protocol behavior still needs confirmation outside the sandbox.
  Minimum pass: connect, evaluate, inspect, xref, trace, and verify the chosen interrupt behavior from Emacs.

- [ ] Reconcile `feature/slime` with `origin/master` before merge.
  As of 2026-03-23, the branch is behind `origin/master` by 2 commits.
  This is not feature work, but it is still outstanding integration work before the branch can be considered finished.
