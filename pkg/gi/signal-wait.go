// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"os"
	"os/signal"
	"syscall"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SignalWait{Function: slip.Function{Name: "signal-wait", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "signal-wait",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "signal",
					Type: "fixnum",
					Text: "signals to wait for.",
				},
			},
			Text: `__signal-wait__ blocks until one of the specified signals is received.`,
			Examples: []string{
				`(signal-wait sigint) ;; waits for the SIGINT to be received`,
			},
		}, &Pkg)
}

// SignalWait represents the signal-wait function.
type SignalWait struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SignalWait) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 {
		slip.PanicArgCount(f, 1, -1)
	}
	var sigs []os.Signal
	for _, a := range args {
		if num, ok := a.(slip.Fixnum); ok {
			sigs = append(sigs, syscall.Signal(num))
		}
	}
	done := make(chan os.Signal, 1)
	signal.Notify(done, sigs...)
	<-done

	return nil
}
