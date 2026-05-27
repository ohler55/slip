// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"os/exec"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var commandFlavor *flavors.Flavor

func defCommand() {
	commandFlavor = flavors.DefFlavor("command",
		map[string]slip.Object{},
		[]string{},
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`Represents a command to start a process.`),
			},
		},
		&Pkg,
	)

	// TBD :init

	commandFlavor.DefMethod(":pid", "", commandPidCaller{})
	flavors.FlosFun("command-pid", ":pid", commandPidCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":process", "", commandProcessCaller{})
	flavors.FlosFun("command-process", ":process", commandProcessCaller{}.FuncDocs(), &Pkg)

	// - :run
	// - :start

	// figure out how stdxxx work vs stdxxpipe
	// - :stdin input-stream
	// - :stdout output-stream
	// - :stderr output-stream
	// - :env
	// - :path
	// - :args
	// - :dir

}

type commandPidCaller struct{}

func (caller commandPidCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":pid", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		result = slip.Fixnum(command.Process.Pid)
	}
	return
}

func (caller commandPidCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":pid",
		Text:   `Returns the command pid.`,
		Return: "fixnum",
	}
}

type commandProcessCaller struct{}

func (caller commandProcessCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":process", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		inst := commandFlavor.MakeInstance().(*flavors.Instance)
		inst.Any = command.Process
		result = inst
	}
	return
}

func (caller commandProcessCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":process",
		Text:   `Returns the command process.`,
		Return: "process",
	}
}
