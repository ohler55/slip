// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"fmt"
	"os/exec"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestProcessRunning(t *testing.T) {
	cmd := exec.Command("sleep", "2")
	_ = cmd.Start()
	defer func() {
		_ = cmd.Process.Kill()
	}()
	pid := cmd.Process.Pid
	(&sliptest.Function{
		Source: fmt.Sprintf("(send (find-process %d) :exit-code)", pid),
		Expect: "-1",
	}).Test(t)
	(&sliptest.Function{
		Source:    fmt.Sprintf("(send (find-process %d) :success)", pid),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    fmt.Sprintf("(send (find-process %d) :signal t)", pid),
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    fmt.Sprintf("(send (find-process %d) :signal 10000)", pid),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    fmt.Sprintf("(send (find-process %d) :system-time)", pid),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    fmt.Sprintf("(send (find-process %d) :user-time)", pid),
		PanicType: slip.ErrorSymbol,
	}).Test(t)

	// A kill after the process has been killed should panic.
	(&sliptest.Function{
		Source: fmt.Sprintf(`(let ((p (find-process %d)))
                               (send p :kill)
                               (send p :wait)
                               (send p :kill))`, pid),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestProcessSuccess(t *testing.T) {
	cmd := exec.Command("sleep", "0.01")
	_ = cmd.Start()
	pid := cmd.Process.Pid
	(&sliptest.Function{
		Source: fmt.Sprintf(`(let ((p (find-process %d)))
                               (send p :wait)
                               (send p :success))`, pid),
		Expect: "t",
	}).Test(t)
}

func TestProcessWaitWait(t *testing.T) {
	cmd := exec.Command("sleep", "0.01")
	_ = cmd.Start()
	pid := cmd.Process.Pid
	(&sliptest.Function{
		Source: fmt.Sprintf(`(let ((p (find-process %d)))
                               (send p :wait)
                               (send p :wait))`, pid),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestProcessWaitSignal(t *testing.T) {
	cmd := exec.Command("sleep", "0.01")
	_ = cmd.Start()
	pid := cmd.Process.Pid
	(&sliptest.Function{
		Source: fmt.Sprintf(`(let ((p (find-process %d)))
                               (send p :wait)
                               (send p :signal 0))`, pid),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestProcessWaitWaitError(t *testing.T) {
	cmd := exec.Command("sleep", "0.01")
	_ = cmd.Start()
	pid := cmd.Process.Pid
	scope := slip.NewScope()
	p := slip.ReadString(fmt.Sprintf("(find-process %d)", pid), scope).Eval(scope, nil)
	scope.Let("p", p)
	_ = cmd.Wait()
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send p :wait)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send p :kill)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
