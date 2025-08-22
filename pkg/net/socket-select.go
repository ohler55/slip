// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketSelect() {
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
					Type: "socket|list-of-socket",
					Text: "sockets to check ready to read",
				},
				{
					Name: "write",
					Type: "socket|list-of-socket",
					Text: "sockets to check ready to write",
				},
				{
					Name: "error",
					Type: "socket|list-of-socket",
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
				`(socket-select (make-instance 'socket :socket 5) :timeout 1) => (#<socket 1234>), nil, nil`,
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
	rlist := socketListArg(s, args[0], "read", depth)
	wlist := socketListArg(s, args[1], "write", depth)
	elist := socketListArg(s, args[2], "error", depth)

	if val, has := slip.GetArgsKeyValue(args[3:], slip.Symbol(":timeout")); has && val != nil {
		if r, ok := val.(slip.Real); ok && 0.0 <= r.RealValue() {
			timeout = time.Duration(r.RealValue() * float64(time.Second))
		} else {
			slip.TypePanic(s, depth, ":timeout", val, "non-negative real")
		}
	}
	rset := setSocketSets(s, rlist, "read", depth)
	wset := setSocketSets(s, wlist, "write", depth)
	eset := setSocketSets(s, elist, "error", depth)

	_ = Select(rset, wset, eset, timeout)

	return slip.Values{
		filterSocketList(rlist, rset),
		filterSocketList(wlist, wset),
		filterSocketList(elist, eset),
	}
}

func socketListArg(s *slip.Scope, arg slip.Object, name string, depth int) (list slip.List) {
	switch ta := arg.(type) {
	case nil:
	case *flavors.Instance:
		list = slip.List{ta}
	case slip.List:
		list = ta
	default:
		slip.TypePanic(s, depth, name, arg, "socket", "list of socket")
	}
	return
}

func setSocketSets(s *slip.Scope, list slip.List, name string, depth int) *FdSet {
	var set FdSet
	for _, val := range list {
		if sock, _ := val.(*flavors.Instance); sock.IsA("socket") {
			if fd, ok := sock.Any.(int); ok {
				set.Set(fd)
			}
		} else {
			slip.TypePanic(s, depth, name, val, "socket", "list of sockets")
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
