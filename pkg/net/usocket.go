// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net"
	"os"
	"strings"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	usocketFlavor *flavors.Flavor
)

func defUsocket() {
	usocketFlavor = flavors.DefFlavor("usocket", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(":network"),
				slip.Symbol(":address"),
				slip.Symbol(":timeout"),
				slip.Symbol(":file"),
				slip.Symbol(":fd"),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A socket wrapper.`),
			},
		},
		&Pkg,
	)
	// usocketFlavor.Final = true
	usocketFlavor.DefMethod(":init", "", usocketInitCaller(true))
}

type usocketInitCaller bool

func (caller usocketInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	obj.Any = nil // TBD net.Conn
	network := "tcp"
	var (
		address string
		timeout time.Duration
	)
	for i := 0; i < len(args); i += 2 {
		key, _ := args[i].(slip.Symbol)
		k := string(key)
		switch {
		case strings.EqualFold(":network", k):
			network = getStrArg(args[i+1], k)
		case strings.EqualFold(":address", k):
			address = getStrArg(args[i+1], k)
		case strings.EqualFold(":timeout", k):
			if r, ok := args[i+1].(slip.Real); ok {
				timeout = time.Duration(r.RealValue() * float64(time.Second))
			} else {
				slip.PanicType(":timeout", args[i+1], "real")
			}
		case strings.EqualFold(":file", k):
			if fs, ok := args[i+1].(*slip.FileStream); ok {
				if c, err := net.FileConn((*os.File)(fs)); err == nil {
					obj.Any = c
				} else {
					panic(err)
				}
			} else {
				slip.PanicType(":file", args[i+1], "file-stream")
			}
		case strings.EqualFold(":fd", k):
			// os.NewFile(fd, some-name)
			// TBD create os.File then net.FileConn
		}
	}
	if obj.Any == nil && 0 < len(address) {
		var err error
		if 0 < timeout {
			obj.Any, err = net.DialTimeout(network, address, timeout)
		} else {
			obj.Any, err = net.Dial(network, address)
		}
		if err != nil {
			panic(err)
		}
	}
	return nil
}

func (caller usocketInitCaller) Docs() string {
	return `__:init__ &key _network_ _address_ _timeout_ _file_ _fd_
   _:network_ [string] must be one of "tcp", "tcp4", "tcp6", "udp", "udp4", "udp6", "ip", "ip4", "ip6", "unix",
"unixgram" or "unixpacket".
   _:address_ [string] the format should be <host>:<port>.
   _:timeout_ [real] the timeout in seconds.
   _:file_ [file-stream] a file-stream to use as socket.
   _:fd_ [fixnum] a file descriptor.


Initializes an instance. If an _address_ is provided then an attempt will be
made connect to that address on the network provided or the default 'tcp'. The
_timeout_ also applies if provided. If a _file_ or _fd_ is provided a socket
is created for the designated file.
`
}
