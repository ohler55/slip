// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"os"
	"syscall"

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
}

type processPidCaller struct{}

func (caller processPidCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":pid", len(args), 0, 0)
	process := self.Any.(*os.Process)

	return slip.Fixnum(process.Pid)
}

func (caller processPidCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":pid",
		Text:   `Returns the cached process pid.`,
		Return: "fixnum",
	}
}

type processKillCaller struct{}

func (caller processKillCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":kill", len(args), 0, 0)
	process := self.Any.(*os.Process)

	if err := process.Kill(); err != nil {
		slip.ErrorPanic(s, depth, "%s", err)
	}
	return nil
}

func (caller processKillCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":kill",
		Text:   `Kill a process and return immediately.`,
		Return: "nil",
	}
}

type processSignalCaller struct{}

func (caller processSignalCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":signal", len(args), 1, 1)
	process := self.Any.(*os.Process)
	sig, ok := args[1].(slip.Fixnum)
	if !ok {
		slip.TypePanic(s, depth, "signal", args[1], "fixnum")
	}
	if err := process.Signal(syscall.Signal(sig)); err != nil {
		slip.ErrorPanic(s, depth, "%s", err)
	}
	return nil
}

func (caller processSignalCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":signal",
		Text: `Signal a process with the _signal_.`,
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
