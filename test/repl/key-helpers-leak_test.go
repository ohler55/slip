// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"runtime"
	"strings"
	"testing"
	"time"
)

// keyEdTest must not leave its own goroutines behind (addendum E5). Each run
// used to leak the output pump blocked in Termock.Output. Only goroutines
// started by keyEdTestSize are counted; the editor's input reader is not
// part of the harness.
func TestKeyEdTestLeavesNoGoroutines(t *testing.T) {
	withKeyBindings(t, "")
	for range 3 {
		keyEdTest(t, []any{startSteps, provide("x"), until("x")})
	}
	var n int
	for range 100 {
		if n = keyHelperGoroutines(); n == 0 {
			break
		}
		time.Sleep(10 * time.Millisecond)
	}
	if 0 < n {
		t.Fatalf("%d keyEdTestSize goroutines still running", n)
	}
}

// keyHelperGoroutines returns the number of running goroutines started by
// keyEdTestSize.
func keyHelperGoroutines() (n int) {
	buf := make([]byte, 1<<20)
	buf = buf[:runtime.Stack(buf, true)]
	for g := range strings.SplitSeq(string(buf), "\n\n") {
		if strings.Contains(g, "keyEdTestSize.func") {
			n++
		}
	}
	return
}
