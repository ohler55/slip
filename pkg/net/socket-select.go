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
			f := SocketSelect{Function: slip.Function{Name: "socket-select", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-select",
			Args: []*slip.DocArg{
				{
					Name: "read",
					Type: "usocket|list-of-usocket",
					Text: "sockets to check ready to read",
				},
				{
					Name: "write",
					Type: "usocket|list-of-usocket",
					Text: "sockets to check ready to write",
				},
				{
					Name: "error",
					Type: "usocket|list-of-usocket",
					Text: "sockets to check for errors",
				},
				{Name: "&key"},
				{
					Name: "timeout",
					Type: "real",
					Text: "the number of seconds to wait before timing out.",
				},
			},
			Return: "list, list, list",
			Text: `__socket-select__ waits for read, write, errors, or a timeout.
Three values, lists of the sockets ready for read, write, or errors is returned.`,
			Examples: []string{
				`(socket-select (make-instance 'usocket :socket 5) :timeout 1) => (#<usocket 1234>), nil, nil`,
			},
		}, &Pkg)
}

// SocketSelect represents the socket-select function.
type SocketSelect struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketSelect) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 3, 5)

	timeout := -time.Second
	rlist := socketListArg(args[0], "read")
	wlist := socketListArg(args[1], "write")
	elist := socketListArg(args[2], "error")

	if val, has := slip.GetArgsKeyValue(args[3:], slip.Symbol(":timeout")); has && val != nil {
		if r, ok := val.(slip.Real); ok && 0.0 <= r.RealValue() {
			timeout = time.Duration(r.RealValue() * float64(time.Second))
		} else {
			slip.PanicType(":timeout", val, "non-negative real")
		}
	}
	rset := setSocketSets(rlist, "read")
	wset := setSocketSets(wlist, "write")
	eset := setSocketSets(elist, "error")

	_ = Select(rset, wset, eset, timeout)

	return slip.Values{
		filterSocketList(rlist, rset),
		filterSocketList(wlist, wset),
		filterSocketList(elist, eset),
	}
}

func socketListArg(arg slip.Object, name string) (list slip.List) {
	switch ta := arg.(type) {
	case nil:
	case *flavors.Instance:
		list = slip.List{ta}
	case slip.List:
		list = ta
	default:
		slip.PanicType(name, arg, "usocket", "list of usocket")
	}
	return
}

func setSocketSets(list slip.List, name string) *FdSet {
	var set FdSet
	for _, val := range list {
		if sock, ok := val.(*flavors.Instance); ok && sock.Flavor == usocketFlavor {
			if fd, ok2 := sock.Any.(int); ok2 {
				set.Set(fd)
			}
		} else {
			slip.PanicType(name, val, "usocket", "list of sockets")
		}
	}
	return &set
}

func filterSocketList(list slip.List, set *FdSet) (out slip.List) {
	for _, val := range list {
		sock := val.(*flavors.Instance)
		if fd, ok := sock.Any.(int); ok {
			if set.IsSet(fd) {
				out = append(out, sock)
			}
		}
	}
	return
}
