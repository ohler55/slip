// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketPair() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketPair{Function: slip.Function{Name: "socket-pair", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-pair",
			Args: []*slip.DocArg{
				{
					Name: "domain",
					Type: "symbol",
					Text: socketArgText("domain", domainMap),
				},
				{
					Name: "type",
					Type: "symbol",
					Text: socketArgText("type", typeMap),
				},
				{
					Name: "protocol",
					Type: "symbol",
					Text: socketArgText("protocol", protocolMap),
				},
				{Name: "&key"},
				{
					Name: "nonblock",
					Type: "boolean",
					Text: "if true the sockets are non blocking.",
				},
				{
					Name: "cloexec",
					Type: "boolean",
					Text: "if true the SOCK CLOEXEC flag is set on the sockets.",
				},
			},
			Return: "socket, socket",
			Text:   `__socket-pair__ returns to connected _socket_ instances.`,
			Examples: []string{
				`(socket-pair :unix :stream nil) => #<socket 1234>, #<socket 1235>`,
			},
		}, &Pkg)
}

// SocketPair represents the socket-pair function.
type SocketPair struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketPair) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 3, 7)

	domain := getSockArgValue(s, "domain", args[0], domainMap, depth)
	typ := getSockArgValue(s, "type", args[1], typeMap, depth)
	var proto int
	if args[2] != nil {
		proto = getSockArgValue(s, "protocol", args[2], protocolMap, depth)
	}
	fds, _ := syscall.Socketpair(domain, typ, proto)
	sock0 := socketFlavor.MakeInstance().(*flavors.Instance)
	sock0.SetSynchronized(true)
	sock0.Any = fds[0]
	sock1 := socketFlavor.MakeInstance().(*flavors.Instance)
	sock1.SetSynchronized(true)
	sock1.Any = fds[1]
	if val, has := slip.GetArgsKeyValue(args[3:], slip.Symbol(":nonblock")); has && val != nil {
		_ = syscall.SetNonblock(fds[0], true)
		_ = syscall.SetNonblock(fds[1], true)
	}
	if val, has := slip.GetArgsKeyValue(args[3:], slip.Symbol(":cloexec")); has && val != nil {
		syscall.CloseOnExec(fds[0])
		syscall.CloseOnExec(fds[1])
	}
	return slip.Values{sock0, sock1}
}
