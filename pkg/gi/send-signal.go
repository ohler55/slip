// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"syscall"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SendSignal{Function: slip.Function{Name: "send-signal", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "send-signal",
			Args: []*slip.DocArg{
				{
					Name: "pid",
					Type: "fixnum",
					Text: "process to send the signal to.",
				},
				{
					Name: "signal",
					Type: "fixnum",
					Text: "signal to send.",
				},
			},
			Text: `__send-signal__ send _signal_ to _pid_.`,
			Examples: []string{
				`(send-signal 12345 sigint) => nil`,
			},
		}, &Pkg)
}

// SendSignal represents the send-signal function.
type SendSignal struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SendSignal) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	pid, ok := args[0].(slip.Fixnum)
	if !ok {
		slip.PanicType("pid", args[0], "fixnum")
	}
	var sig slip.Fixnum
	if sig, ok = args[1].(slip.Fixnum); !ok {
		slip.PanicType("signal", args[1], "fixnum")
	}
	if err := syscall.Kill(int(pid), syscall.Signal(sig)); err != nil {
		panic(err)
	}
	return nil
}
