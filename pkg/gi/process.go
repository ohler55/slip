// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"os"
	"syscall"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var processFlavor *flavors.Flavor

func defProcess() {
	processFlavor = flavors.DefFlavor("process",
		map[string]slip.Object{},
		[]string{},
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`Represents a system process.`),
			},
		},
		&Pkg,
	)
	processFlavor.Final = true
	processFlavor.GoMakeOnly = true

	processFlavor.DefMethod(":pid", "", processPidCaller{})
	flavors.FlosFun("process-pid", ":pid", processPidCaller{}.FuncDocs(), &Pkg)

	processFlavor.DefMethod(":kill", "", processKillCaller{})
	flavors.FlosFun("process-kill", ":kill", processKillCaller{}.FuncDocs(), &Pkg)

	processFlavor.DefMethod(":signal", "", processSignalCaller{})
	flavors.FlosFun("process-signal", ":signal", processSignalCaller{}.FuncDocs(), &Pkg)

	processFlavor.DefMethod(":wait", "", processWaitCaller{})
	flavors.FlosFun("process-wait", ":wait", processWaitCaller{}.FuncDocs(), &Pkg)

	processFlavor.DefMethod(":exited", "", processExitedCaller{})
	flavors.FlosFun("process-exited", ":exited", processExitedCaller{}.FuncDocs(), &Pkg)

	processFlavor.DefMethod(":success", "", processSuccessCaller{})
	flavors.FlosFun("process-success", ":success", processSuccessCaller{}.FuncDocs(), &Pkg)

	processFlavor.DefMethod(":exit-code", "", processExitCodeCaller{})
	flavors.FlosFun("process-exit-code", ":exit-code", processExitCodeCaller{}.FuncDocs(), &Pkg)

	processFlavor.DefMethod(":system-time", "", processSystemTimeCaller{})
	flavors.FlosFun("process-system-time", ":system-time", processSystemTimeCaller{}.FuncDocs(), &Pkg)

	processFlavor.DefMethod(":user-time", "", processUserTimeCaller{})
	flavors.FlosFun("process-user-time", ":user-time", processUserTimeCaller{}.FuncDocs(), &Pkg)
}

type processPidCaller struct{}

func (caller processPidCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":pid", len(args), 0, 0)
	var pid slip.Fixnum
	switch pa := self.Any.(type) {
	case *os.Process:
		pid = slip.Fixnum(pa.Pid)
	case *os.ProcessState:
		pid = slip.Fixnum(pa.Pid())
	}
	return pid
}

func (caller processPidCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":pid",
		Text:   `returns the process pid.`,
		Return: "fixnum",
	}
}

type processKillCaller struct{}

func (caller processKillCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":kill", len(args), 0, 0)
	switch pa := self.Any.(type) {
	case *os.Process:
		if err := pa.Kill(); err != nil {
			slip.ErrorPanic(s, depth, "%s", err)
		}
	case *os.ProcessState:
		slip.ErrorPanic(s, depth, "Process with pid %d is no longer running.", pa.Pid())
	}
	return nil
}

func (caller processKillCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":kill",
		Text:   `a process and return immediately.`,
		Return: "nil",
	}
}

type processSignalCaller struct{}

func (caller processSignalCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":signal", len(args), 1, 1)
	sig, ok := args[0].(slip.Fixnum)
	if !ok {
		slip.TypePanic(s, depth, "signal", args[0], "fixnum")
	}
	switch pa := self.Any.(type) {
	case *os.Process:
		if err := pa.Signal(syscall.Signal(sig)); err != nil {
			slip.ErrorPanic(s, depth, "%s", err)
		}
	case *os.ProcessState:
		slip.ErrorPanic(s, depth, "Process with pid %d is no longer running.", pa.Pid())
	}
	return nil
}

func (caller processSignalCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":signal",
		Text: `a process with the _signal_.`,
		Args: []*slip.DocArg{
			{
				Name: "signal",
				Type: "fixnum",
				Text: "The signal to send.",
			},
		},
		Return: "nil",
	}
}

type processWaitCaller struct{}

func (caller processWaitCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":wait", len(args), 0, 0)
	var code slip.Fixnum
	switch pa := self.Any.(type) {
	case *os.Process:
		ps, err := pa.Wait()
		if err != nil {
			slip.ErrorPanic(s, depth, "%s", err)
		}
		code = slip.Fixnum(ps.ExitCode())
		self.Any = ps
	case *os.ProcessState:
		slip.ErrorPanic(s, depth, "Process with pid %d is no longer running.", pa.Pid())
	}
	return code
}

func (caller processWaitCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":wait",
		Text:   `for a process to complete and return the exit code.`,
		Return: "symbol",
	}
}

type processExitedCaller struct{}

func (caller processExitedCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":exited", len(args), 0, 0)
	if _, ok := self.Any.(*os.ProcessState); ok {
		result = slip.True
	}
	return
}

func (caller processExitedCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":exited",
		Text:   `returns true if the process has exited. __:wait__ must be called before this.`,
		Return: "boolean",
	}
}

type processSuccessCaller struct{}

func (caller processSuccessCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":success", len(args), 0, 0)
	switch pa := self.Any.(type) {
	case *os.Process:
		slip.ErrorPanic(s, depth,
			"Process with pid %d is still running or __:wait__ has net yet been called", pa.Pid)
	case *os.ProcessState:
		if pa.Success() {
			result = slip.True
		}
	}
	return
}

func (caller processSuccessCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":success",
		Text:   `returns true if the process exited successfully.`,
		Return: "boolean",
	}
}

type processExitCodeCaller struct{}

func (caller processExitCodeCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":exit-code", len(args), 0, 0)
	switch pa := self.Any.(type) {
	case *os.Process:
		result = slip.Fixnum(-1)
	case *os.ProcessState:
		result = slip.Fixnum(pa.ExitCode())
	}
	return
}

func (caller processExitCodeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":exit-code",
		Text:   `returns the process exit code if completed and -1 if still running or exited by a signal`,
		Return: "fixnum",
	}
}

type processSystemTimeCaller struct{}

func (caller processSystemTimeCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":system-time", len(args), 0, 0)
	switch pa := self.Any.(type) {
	case *os.Process:
		slip.ErrorPanic(s, depth,
			"Process with pid %d is still running or __:wait__ has net yet been called", pa.Pid)
	case *os.ProcessState:
		result = slip.DoubleFloat(float64(pa.SystemTime()) / float64(time.Second))
	}
	return
}

func (caller processSystemTimeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":system-time",
		Text:   `returns the system time if the process has finished. The __:wait__ method must be called first.`,
		Return: "float",
	}
}

type processUserTimeCaller struct{}

func (caller processUserTimeCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":user-time", len(args), 0, 0)
	switch pa := self.Any.(type) {
	case *os.Process:
		slip.ErrorPanic(s, depth,
			"Process with pid %d is still running or __:wait__ has net yet been called", pa.Pid)
	case *os.ProcessState:
		result = slip.DoubleFloat(float64(pa.UserTime()) / float64(time.Second))
	}
	return
}

func (caller processUserTimeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":user-time",
		Text:   `returns the user time if the process has finished. The __:wait__ method must be called first.`,
		Return: "float",
	}
}
