// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"os"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	socketFlavor *flavors.Flavor
)

func defSocket() {
	socketFlavor = flavors.DefFlavor("socket", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(":socket"),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A socket wrapper.`),
			},
		},
		&Pkg,
	)
	socketFlavor.DefMethod(":init", "", socketInitCaller{})
	socketFlavor.DefMethod(":socket", "", socketSocketCaller{})
	socketFlavor.DefMethod(":set-socket", "", socketSetSocketCaller{})
	socketFlavor.DefMethod(":state", "", socketStateCaller{})
	socketFlavor.DefMethod(":close", "", socketCloseCaller{})
	socketFlavor.DefMethod(":local-address", "", socketLocalAddressCaller{})
	socketFlavor.DefMethod(":local-name", "", socketLocalNameCaller{})
	socketFlavor.DefMethod(":local-port", "", socketLocalPortCaller{})
	socketFlavor.DefMethod(":peer-address", "", socketPeerAddressCaller{})
	socketFlavor.DefMethod(":peer-name", "", socketPeerNameCaller{})
	socketFlavor.DefMethod(":peer-port", "", socketPeerPortCaller{})
	socketFlavor.DefMethod(":send", "", socketSendCaller{})
	socketFlavor.DefMethod(":receive", "", socketReceiveCaller{})
	socketFlavor.DefMethod(":stream", "", socketStreamCaller{})
	socketFlavor.DefMethod(":option", "", socketOptionCaller{})
	socketFlavor.DefMethod(":set-option", "", socketSetOptionCaller{})
}

type socketInitCaller struct{}

func (caller socketInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	for i := 0; i < len(args); i += 2 {
		key, _ := args[i].(slip.Symbol)
		if strings.EqualFold(":socket", string(key)) {
			socketSetSocketCaller{}.Call(s, slip.List{args[i+1]}, 0)
		}
	}
	return nil
}

func (caller socketInitCaller) Docs() string {
	return `__:init__ &key _socket_
   _:socket_ [socket-stream|file-stream|fixnum] a bidirectional stream to use for the instance's socket.


Initializes an instance with the provided _socket_.
`
}

type socketSocketCaller struct{}

func (caller socketSocketCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.ArgCountCheck(self, args, 0, 0)
	if self.Any != nil {
		result = slip.Fixnum(self.Any.(int))
	}
	return
}

func (caller socketSocketCaller) Docs() string {
	return `__:socket__


Returns the stream of the instance or nil if not connected.
`
}

type socketSetSocketCaller struct{}

func (caller socketSetSocketCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.ArgCountCheck(self, args, 1, 1)
	switch ta := args[0].(type) {
	case *slip.FileStream:
		self.Any = int((*os.File)(ta).Fd())
	case slip.Fixnum:
		self.Any = int(ta)
	default:
		slip.PanicType("socket", ta, "fixnum", "socket-stream", "file-stream")
	}
	return nil
}

func (caller socketSetSocketCaller) Docs() string {
	return `__:set-socket__ _stream_
   _stream_ [socket-stream|file-stream|fixnum] an stream returned from a call to :socket, a socket file,
or a fixnum file descriptor


Sets the underlying stream of the instance.
`
}
