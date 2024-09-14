// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"sort"
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
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
			Return: "usocket, usocket",
			Text:   `__socket-pair__ returns to connected _usocket_ instances.`,
			Examples: []string{
				`(socket-pair :unix :stream nil) => #<usocket 1234>, #<usocket 1235>`,
			},
		}, &Pkg)
}

// SocketPair represents the socket-pair function.
type SocketPair struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketPair) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 3, 7)

	domain := getSockArgValue("domain", args[0], domainMap)
	typ := getSockArgValue("type", args[1], typeMap)
	var proto int
	if args[2] != nil {
		proto = getSockArgValue("protocol", args[2], protocolMap)
	}
	fds, _ := syscall.Socketpair(domain, typ, proto)
	sock0 := usocketFlavor.MakeInstance().(*flavors.Instance)
	sock0.Any = fds[0]
	sock1 := usocketFlavor.MakeInstance().(*flavors.Instance)
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

func getSockArgValue(name string, arg slip.Object, argMap map[slip.Symbol]int) int {
	sym, ok := arg.(slip.Symbol)
	if !ok || argMap[sym] == 0 {
		keys := make([]string, 0, len(argMap))
		for sym := range argMap {
			keys = append(keys, string(sym))
		}
		sort.Strings(keys)
		slip.PanicType(name, arg, keys...)
	}
	return argMap[sym]
}

func socketArgText(name string, argMap map[slip.Symbol]int) string {
	var b []byte
	b = append(b, "the socket "...)
	b = append(b, name...)
	b = append(b, ". Valid options are:"...)
	keys := make([]string, 0, len(argMap))
	for sym := range argMap {
		keys = append(keys, string(sym))
	}
	sort.Strings(keys)
	for _, key := range keys {
		b = append(b, ' ')
		b = append(b, key...)
	}
	return string(b)
}
