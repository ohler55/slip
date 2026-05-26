// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"os"
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defFindProcess() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FindProcess{Function: slip.Function{Name: "find-process", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "find-process",
			Args: []*slip.DocArg{
				{
					Name: "pid",
					Type: "fixnum",
					Text: "Pid of the process to find.",
				},
			},
			Return: "process",
			Text: `__find-process__ find the process with a pid of _pid_ and return an
instance of the __process__ flavor or __nil__ if no process is found.`,
			Examples: []string{
				`(find-process  12345) => #<process 12345>`,
			},
		}, &Pkg)
}

// FindProcess represents the find-process function.
type FindProcess struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FindProcess) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	pid, ok := args[0].(slip.Fixnum)
	if !ok {
		slip.TypePanic(s, depth, "pid", args[0], "fixnum")
	}
	proc, _ := os.FindProcess(int(pid))

	if proc == nil || proc.Signal(syscall.Signal(0)) != nil {
		return nil
	}
	inst := processFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = proc

	return inst
}
