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

func defSocket() *flavors.Flavor {
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
				slip.String(`A socket wrapper that mostly follows the sbcl socket implementation
and is based on the unix or BSD socket model.`),
			},
		},
		&Pkg,
	)
	socketFlavor.DefMethod(":init", "", socketInitCaller{})
	socketFlavor.DefMethod(":accept", "", socketAcceptCaller{})
	socketFlavor.DefMethod(":address", "", socketAddressCaller{})
	socketFlavor.DefMethod(":bind", "", socketBindCaller{})
	socketFlavor.DefMethod(":close", "", socketCloseCaller{})
	socketFlavor.DefMethod(":connect", "", socketConnectCaller{})
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

	return socketFlavor
}

type socketInitCaller struct{}

func (caller socketInitCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
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
		domain = getSockArgValue(s, ":domain", val, domainMap, depth)
	}
	if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":type")); has {
		typ = getSockArgValue(s, ":type", val, typeMap, depth)
	}
	if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":protocol")); has && val != nil {
		pro = getSockArgValue(s, ":protocol", val, protocolMap, depth)
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

func (caller socketInitCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: `Initializes an instance with the provided _socket_ or create a new socket with the
_domain_, _type_ and optional _protocol_.`,
		Args: []*slip.DocArg{
			{Name: "&key"},
			{
				Name: ":socket",
				Type: "fixnum",
				Text: `A bidirectional socket file descriptor to use for the instance's socket.`,
			},
			{
				Name: ":domain",
				Type: "keyword",
				Text: "A domain for a new socket (e.g., :inet)",
			},
			{
				Name: ":type",
				Type: "keyword",
				Text: "A socket type (e.g., :stream or :datagram)",
			},
			{
				Name: ":protocol",
				Type: "keyword",
				Text: `A protocol for a new socket (e.g., :ipV4)`,
			},
		},
	}
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

func (caller socketSocketCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":socket",
		Text:   `Returns the socket file descriptor of the instance or nil if not open.`,
		Return: "nil|fixnum",
	}
}

type socketSetSocketCaller struct{}

func (caller socketSetSocketCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.ArgCountCheck(self, args, 1, 1)
	switch ta := args[0].(type) {
	case *slip.FileStream:
		self.Any = int((*os.File)(ta).Fd())
	case slip.Fixnum:
		self.Any = int(ta)
	default:
		slip.TypePanic(s, depth, "socket", ta, "fixnum", "socket-stream", "file-stream")
	}
	return nil
}

func (caller socketSetSocketCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set-socket",
		Text: `Sets the underlying socket file descriptor of the instance.`,
		Args: []*slip.DocArg{
			{
				Name: "stream",
				Type: "socket-stream|file-stream|fixnum",
				Text: `A stream returned from a call to :socket, a socket file,
or a fixnum file descriptor.`,
			},
		},
	}
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

func (caller socketTypeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":type",
		Text:   `Returns the socket type or nil if not open.`,
		Return: "nil|symbol",
	}
}
