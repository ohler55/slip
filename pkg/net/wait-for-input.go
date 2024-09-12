// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WaitForInput{Function: slip.Function{Name: "wait-for-input", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "wait-for-input",
			Args: []*slip.DocArg{
				{
					Name: "sockets",
					Type: "usocket|list-of-usocket",
					Text: "to wait for input from",
				},
				{Name: "&key"},
				{
					Name: "timeout",
					Type: "real",
					Text: "the number of seconds to wait before timing out.",
				},
				{
					Name: "ready-only",
					Type: "boolean",
					Text: `if true only the ready sockets are returned. If nil then all are returned.
The default is _t_.`,
				},
			},
			Return: "nil|",
			Text: `__wait-for-input__ waits for input on any of _sockets_ or a timeout.
A list of the sockets ready for input and the time remaining in the timeout is returned. If
the call timed out then the second both argument are nil.`,
			Examples: []string{
				`(wait-for-input (make-instance 'usocket :socket 5) :timeout 1) => (#<usocket 1234>), 0.5`,
			},
		}, &Pkg)
}

// WaitForInput represents the wait-for-input function.
type WaitForInput struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WaitForInput) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 5)
	var sockets slip.List
	timeout := -time.Second
	readyOnly := true
	switch ta := args[0].(type) {
	case *flavors.Instance:
		sockets = slip.List{ta}
	case slip.List:
		sockets = ta
	default:
		slip.PanicType("sockets", args[0], "usocket", "list of usocket")
	}
	args = args[1:]
	if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":timeout")); has && val != nil {
		if r, ok := val.(slip.Real); ok && 0.0 <= r.RealValue() {
			timeout = time.Duration(r.RealValue() * float64(time.Second))
		} else {
			slip.PanicType(":timeout", val, "non-negative real")
		}
	}
	if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":ready-only")); has {
		readyOnly = val != nil
	}
	var rset FdSet
	for _, val := range sockets {
		if sock, ok := val.(*flavors.Instance); ok && sock.Flavor == usocketFlavor {
			if fd, ok2 := sock.Any.(int); ok2 {
				rset.Set(fd)
			}
		} else {
			slip.PanicType("sockets", val, "usocket", "list of sockets")
		}
	}
	start := time.Now()
	// With the prechecks the only error could be the kernel memory allocator.
	_ = Select(&rset, nil, nil, timeout)

	if readyOnly {
		var ready slip.List
		for _, val := range sockets {
			sock := val.(*flavors.Instance)
			if fd, ok := sock.Any.(int); ok {
				if rset.IsSet(fd) {
					ready = append(ready, sock)
				}
			}
		}
		sockets = ready
	}
	var remain slip.Object
	if 0 < timeout {
		rem := timeout - time.Since(start)
		if 0 < rem {
			remain = slip.DoubleFloat(float64(rem) / float64(time.Second))
		}
	}
	return slip.Values{sockets, remain}
}
