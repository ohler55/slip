// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"os"
	"syscall"

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
				slip.Symbol(":domain"),
				slip.Symbol(":type"),
				slip.Symbol(":protocol"),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A socket wrapper what includes read and write indicators to facilitate
shutting down read and write on the socket.`),
			},
		},
		&Pkg,
	)
	socketFlavor.DefMethod(":init", "", socketInitCaller{})
	socketFlavor.DefMethod(":accept", "", socketAcceptCaller{})
	socketFlavor.DefMethod(":address", "", socketAddressCaller{})
	socketFlavor.DefMethod(":bind", "", socketBindCaller{})
	socketFlavor.DefMethod(":close", "", socketCloseCaller{})
	socketFlavor.DefMethod(":listen", "", socketListenCaller{})
	socketFlavor.DefMethod(":make-stream", "", socketMakeStreamCaller{})
	socketFlavor.DefMethod(":name", "", socketNameCaller{})
	socketFlavor.DefMethod(":open-p", "", socketOpenpCaller{})
	socketFlavor.DefMethod(":option", "", socketOptionCaller{})
	socketFlavor.DefMethod(":peer-address", "", socketPeerAddressCaller{})
	socketFlavor.DefMethod(":peer-name", "", socketPeerNameCaller{})
	socketFlavor.DefMethod(":peer-port", "", socketPeerPortCaller{})
	socketFlavor.DefMethod(":port", "", socketPortCaller{})
	socketFlavor.DefMethod(":receive", "", socketReceiveCaller{})
	socketFlavor.DefMethod(":send", "", socketSendCaller{})
	socketFlavor.DefMethod(":set-option", "", socketSetOptionCaller{})
	socketFlavor.DefMethod(":set-socket", "", socketSetSocketCaller{})
	socketFlavor.DefMethod(":shutdown", "", socketShutdownCaller{})
	socketFlavor.DefMethod(":socket", "", socketSocketCaller{})
	socketFlavor.DefMethod(":state", "", socketStateCaller{})
	socketFlavor.DefMethod(":stream", "", socketStreamCaller{})
	socketFlavor.DefMethod(":type", "", socketTypeCaller{})
}

type socketInitCaller struct{}

func (caller socketInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":socket")); has {
		socketSetSocketCaller{}.Call(s, slip.List{val}, 0)
		return nil
	}
	var (
		domain int
		typ    int
		pro    int
	)
	if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":domain")); has {
		domain = getSockArgValue(":domain", val, domainMap)
	}
	if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":type")); has {
		typ = getSockArgValue(":type", val, typeMap)
	}
	if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":protocol")); has && val != nil {
		pro = getSockArgValue(":protocol", val, protocolMap)
	}
	if domain != 0 && typ != 0 {
		if fd, err := syscall.Socket(domain, typ, pro); err == nil {
			self := s.Get("self").(*flavors.Instance)
			self.Any = fd
		} else {
			panic(err)
		}
	}
	return nil
}

func (caller socketInitCaller) Docs() string {
	return `__:init__ &key _socket_
   _:socket_ [fixnum] a bidirectional socket file descriptor to use for the instance's socket.
   _:domain_ [keyword] a domain for a new socket (e.g., :inet)
   _:type_ [keyword] a socket type (e.g., :stream or :datagram)
   _:protocol_ [keyword] a protocol for a new socket, defaults to nil or 0


Initializes an instance with the provided _socket_ or create a new socket with the
_domain_, _type_ and optional _protocol_.
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


Returns the socket file descriptor of the instance or nil if not open.
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


Sets the underlying socket file descriptor of the instance.
`
}

type socketTypeCaller struct{}

func (caller socketTypeCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.ArgCountCheck(self, args, 0, 0)
	if self.Any != nil {
		if val, err := syscall.GetsockoptInt(self.Any.(int), syscall.SOL_SOCKET, syscall.SO_TYPE); err == nil {
			for typ, num := range typeMap {
				if num == val {
					result = typ
					break
				}
			}
		}
	}
	return
}

func (caller socketTypeCaller) Docs() string {
	return `__:type__


Returns the socket type or nil if not open.
`
}
