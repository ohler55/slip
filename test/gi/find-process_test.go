// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"fmt"
	"os/exec"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFindProcessOk(t *testing.T) {
	cmd := exec.Command("sleep", "2")
	_ = cmd.Start()
	pid := cmd.Process.Pid
	(&sliptest.Function{
		Source: fmt.Sprintf(`(let ((p (find-process %d)))
                               (list (send p :pid)
                                     (send p :signal 0)
                                     (send p :kill)
                                     (send p :wait)
                                     (send p :exited)
                                     (send p :success)
                                     (send p :exit-code)
                                     (send p :system-time)
                                     (send p :user-time)
                                     (send p :pid)))`, pid),
		Validate: func(t *testing.T, v slip.Object) {
			list, _ := v.(slip.List)
			tt.Equal(t, 10, len(list))
			tt.Equal(t, slip.Fixnum(pid), list[0])
			tt.Nil(t, list[1])                    // return from signal
			tt.Nil(t, list[2])                    // return from :kill
			tt.Equal(t, slip.Fixnum(-1), list[3]) // return from wait, expect -1 since killed
			tt.Equal(t, slip.True, list[4])
			tt.Nil(t, list[5]) // killed so not success
			tt.Equal(t, slip.Fixnum(-1), list[6])

			tt.SameType(t, slip.DoubleFloat(1.1), list[7])
			dur := list[7].(slip.Float).RealValue()
			tt.Equal(t, true, 0.0 <= dur && dur < 1.0)

			tt.SameType(t, slip.DoubleFloat(1.1), list[8])
			dur = list[8].(slip.Float).RealValue()
			tt.Equal(t, true, 0.0 <= dur && dur < 1.0)

			tt.Equal(t, slip.Fixnum(pid), list[9])
		},
	}).Test(t)
}

func TestFindProcessNotFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(find-process 0)",
		Expect: "nil",
	}).Test(t)
}

func TestFindProcessBadPid(t *testing.T) {
	(&sliptest.Function{
		Source:    "(find-process t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

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
