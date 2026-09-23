// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
)

// keyStepTimeout bounds each expect/until step in keyEdTest so a missing or
// broken feature fails the test instead of hanging the whole run.
const keyStepTimeout = 3 * time.Second

// untilWithout waits for target like until but fails if forbid shows up
// first.
type untilWithout struct {
	target string
	forbid string
}

// keyEval evaluates src in a fresh scope and returns the printed result. An
// error fails the test rather than aborting the whole run.
func keyEval(t *testing.T, src string) string {
	t.Helper()
	defer func() {
		if r := recover(); r != nil {
			if p, ok := r.(*slip.Panic); ok {
				t.Fatalf("%s failed: %s", src, p.Error())
			}
			t.Fatalf("%s failed: %v", src, r)
		}
	}()
	scope := slip.NewScope()
	result := slip.ReadString(src, scope).Eval(scope, nil)
	return slip.ObjectString(result)
}

// keyError evaluates src, which must panic with a slip error, and returns
// that error.
func keyError(t *testing.T, src string) (p *slip.Panic) {
	t.Helper()
	func() {
		defer func() {
			r := recover()
			var ok bool
			if p, ok = r.(*slip.Panic); !ok {
				t.Fatalf("expected an error from %s, got %v (%T)", src, r, r)
			}
		}()
		scope := slip.NewScope()
		slip.ReadString(src, scope).Eval(scope, nil)
	}()
	return
}

// resetKeyBindings clears all user key bindings. Every test that changes the
// bindings defers this so tests stay order independent.
func resetKeyBindings(t *testing.T) {
	t.Helper()
	_ = keyEval(t, `(setq *repl-key-bindings* nil)`)
}

// withKeyBindings evaluates setup (usually repl-bind-key calls) and registers
// a cleanup that clears the bindings again.
func withKeyBindings(t *testing.T, setup string) {
	t.Helper()
	resetKeyBindings(t)
	t.Cleanup(func() { resetKeyBindings(t) })
	if 0 < len(setup) {
		_ = keyEval(t, setup)
	}
}

// keyEdTest is edTest with a timeout on every step. The script runs in a
// goroutine; output is pumped through a channel so steps can give up.
func keyEdTest(t *testing.T, script []any) {
	keyEdTestSize(t, 40, 80, script)
}

// keyEdTestSize is keyEdTest with a terminal of the given size.
func keyEdTestSize(t *testing.T, high, wide int, script []any) {
	err := os.RemoveAll("config/history")
	tt.Nil(t, err)
	err = os.RemoveAll("config/config.lisp")
	tt.Nil(t, err)
	tm := repl.NewTermock(high, wide)
	defer repl.SetSizer(nil)
	repl.SetSizer(tm)
	scope := repl.GetScope()
	scope.Set(slip.Symbol("x"), slip.Fixnum(3))
	scope.Set(slip.Symbol("*standard-output*"), tm)
	scope.Set(slip.Symbol("*standard-input*"), tm)
	repl.SetConfigDir("config")
	scope.Set(slip.Symbol("*repl-editor*"), slip.True)

	out := make(chan string, 4096)
	go func() {
		for {
			out <- tm.Output()
		}
	}()
	done := make(chan string, 1)
	exited := make(chan bool)
	go func() {
		done <- runKeySteps(tm, out, script)
		// Keep sending C-c until the REPL exits. A single C-c is not enough
		// if a failing binding left the editor in a prefix mode.
		for {
			tm.Input("\x03")
			select {
			case <-exited:
				return
			case <-time.After(keyStepTimeout / 4):
			}
		}
	}()
	repl.Run()
	close(exited)

	var failure string
	select {
	case failure = <-done:
	case <-time.After(keyStepTimeout * 4):
		failure = "script did not finish"
	}
	// Drain until the REPL says goodbye so the next test starts clean.
	for bye := false; !bye; {
		select {
		case s := <-out:
			bye = match("/Bye/", s)
		case <-time.After(keyStepTimeout):
			bye = true
		}
	}
	if 0 < len(failure) {
		t.Fatal(failure)
	}
}

// runKeySteps runs the script steps and returns a failure description or an
// empty string.
func runKeySteps(tm *repl.Termock, out chan string, script []any) string {
	next := func() (string, bool) {
		select {
		case s := <-out:
			if testing.Verbose() {
				fmt.Printf(">>> %q\n", s)
			}
			return s, true
		case <-time.After(keyStepTimeout):
			return "", false
		}
	}
	for _, x := range script {
		switch tx := x.(type) {
		case expect:
			s, ok := next()
			if !ok {
				return fmt.Sprintf("timed out expecting %q", tx)
			}
			if !match(string(tx), s) {
				return fmt.Sprintf("expected %q, got %q", tx, s)
			}
		case until:
			for {
				s, ok := next()
				if !ok {
					return fmt.Sprintf("timed out waiting for %q", tx)
				}
				if match(string(tx), s) {
					break
				}
			}
		case untilWithout:
			for {
				s, ok := next()
				if !ok {
					return fmt.Sprintf("timed out waiting for %q", tx.target)
				}
				if match(tx.forbid, s) {
					return fmt.Sprintf("found %q before %q", s, tx.target)
				}
				if match(tx.target, s) {
					break
				}
			}
		case provide:
			if testing.Verbose() {
				fmt.Printf("<<< %q\n", tx)
			}
			tm.Input(string(tx))
			time.Sleep(time.Millisecond * 10)
		case func(string) bool:
			// Feed each output chunk to the function until it returns true.
			for {
				s, ok := next()
				if !ok {
					return "timed out in a step function"
				}
				if tx(s) {
					break
				}
			}
		case comment:
			if testing.Verbose() {
				fmt.Printf("### %s\n", tx)
			}
		case []any:
			if failure := runKeySteps(tm, out, tx); 0 < len(failure) {
				return failure
			}
		}
	}
	return ""
}

// keyEdSeq is testEditorSeq on top of keyEdTest: provide seq then wait for
// each expected output in order.
func keyEdSeq(t *testing.T, seq string, expects ...string) {
	script := []any{startSteps, provide(seq)}
	for _, x := range expects {
		script = append(script, until(x))
	}
	keyEdTest(t, script)
}

// keyChunks splits text into provide steps short enough for the editor's
// 32 byte key buffer (a known limit, out of scope for this feature).
func keyChunks(text string) []any {
	var steps []any
	for 0 < len(text) {
		n := min(len(text), 16)
		steps = append(steps, provide(text[:n]))
		text = text[n:]
	}
	return steps
}
