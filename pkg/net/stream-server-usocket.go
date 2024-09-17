// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	streamServerUsocketFlavor *flavors.Flavor
)

func defStreamServerUsocket() {
	streamServerUsocketFlavor = flavors.DefFlavor("stream-server-usocket",
		map[string]slip.Object{
			"host":            nil,
			"port":            nil,
			"function":        nil,
			"arguments":       nil,
			"in-new-thread":   nil,
			"protocol":        slip.Symbol(":stream"),
			"timeout":         slip.Fixnum(1),
			"max-buffer-size": slip.Fixnum(65507),
			"element-type":    nil,
			"reuse-address":   slip.True,
			"multi-threading": nil,
			"name":            nil,
			"workers":         nil,
			"domain":          slip.Symbol(":inet"),
		},
		[]string{"usocket"},
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A socket server.`),
			},
			slip.Symbol(":inittable-instance-variables"),
			slip.Symbol(":gettable-instance-variables"),
		},
		&Pkg,
	)
	streamServerUsocketFlavor.DefMethod(":init", "", streamServerUsocketInitCaller{})
}

type streamServerUsocketInitCaller struct{}

func (caller streamServerUsocketInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	startSocketServer(args[0].(*flavors.Instance))

	return nil
}

func (caller streamServerUsocketInitCaller) Docs() string {
	return `__:init__


Starts a stream-server-usocket listening with the configured instance variables.
`
}

func startSocketServer(self *flavors.Instance) {
	v, _ := self.LocalGet(slip.Symbol("domain"))
	sym, _ := v.(slip.Symbol)
	domain := domainMap[sym]

	v, _ = self.LocalGet(slip.Symbol("protocol"))
	sym, _ = v.(slip.Symbol)
	typ := typeMap[sym]

	fd, err := syscall.Socket(domain, typ, 0)
	if err != nil {
		panic(err)
	}
	self.Any = fd

	// TBD
	// set options
	//  host
	//  name - Sethostname()
	// loop calling accept, either inline or in go routine

	// TBD for the multi-threading option make a new go routine
	//   add worker option for channel and workers
}
