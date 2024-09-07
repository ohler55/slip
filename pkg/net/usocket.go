// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"fmt"
	"net"
	"os"
	"strings"

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
				slip.Symbol(":socket"),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A socket wrapper.`),
			},
		},
		&Pkg,
	)
	usocketFlavor.DefMethod(":init", "", usocketInitCaller{})
	usocketFlavor.DefMethod(":socket", "", usocketSocketCaller{})
	usocketFlavor.DefMethod(":set-socket", "", usocketSetSocketCaller{})
	usocketFlavor.DefMethod(":state", "", usocketStateCaller{})
	usocketFlavor.DefMethod(":close", "", usocketCloseCaller{})
	usocketFlavor.DefMethod(":local-address", "", usocketLocalAddressCaller{})
	usocketFlavor.DefMethod(":local-name", "", usocketLocalNameCaller{})
	usocketFlavor.DefMethod(":local-port", "", usocketLocalPortCaller{})
	usocketFlavor.DefMethod(":peer-address", "", usocketPeerAddressCaller{})
	usocketFlavor.DefMethod(":peer-name", "", usocketPeerNameCaller{})
	usocketFlavor.DefMethod(":peer-port", "", usocketPeerPortCaller{})
	usocketFlavor.DefMethod(":send", "", usocketSendCaller{})
	usocketFlavor.DefMethod(":receive", "", usocketReceiveCaller{})
	usocketFlavor.DefMethod(":stream", "", usocketStreamCaller{})
}

type usocketInitCaller struct{}

func (caller usocketInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	for i := 0; i < len(args); i += 2 {
		key, _ := args[i].(slip.Symbol)
		if strings.EqualFold(":socket", string(key)) {
			usocketSetSocketCaller{}.Call(s, slip.List{args[i+1]}, 0)
		}
	}
	return nil
}

func (caller usocketInitCaller) Docs() string {
	return `__:init__ &key _socket_
   _:socket_ [io-stream|file-stream|fixnum] a bidirectional stream to use for the instance's socket.


Initializes an instance with the provided _socket_.
`
}

type usocketSocketCaller struct{}

func (caller usocketSocketCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.ArgCountCheck(self, args, 0, 0)
	if self.Any != nil {
		result = &slip.IOStream{RW: self.Any.(net.Conn)}
	}
	return
}

func (caller usocketSocketCaller) Docs() string {
	return `__:socket__


Returns the stream of the instance or nil if not connected.
`
}

type usocketSetSocketCaller struct{}

func (caller usocketSetSocketCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.ArgCountCheck(self, args, 1, 1)
	switch ta := args[0].(type) {
	case *slip.IOStream:
		if _, ok := ta.RW.(net.Conn); ok {
			self.Any = ta.RW
		} else {
			slip.PanicType("socket", ta, "io-stream", "file-stream")
		}
	case *slip.FileStream:
		if c, err := net.FileConn((*os.File)(ta)); err == nil {
			self.Any = c
		} else {
			panic(err)
		}
	case slip.Fixnum:
		if f := os.NewFile(uintptr(ta), fmt.Sprintf("file-%d", ta)); f != nil {
			defer func() { _ = f.Close() }()
			var err error
			if self.Any, err = net.FileConn(f); err != nil {
				panic(err)
			}
		} else {
			slip.NewPanic("%d is not a valid file descriptor", ta)
		}
	default:
		slip.PanicType("socket", ta, "io-stream", "file-stream")
	}
	return nil
}

func (caller usocketSetSocketCaller) Docs() string {
	return `__:set-socket__ _stream_
   _stream_ [io-stream|file-stream|fixnum] an io-stream returned from a call to :socket, a socket file,
or a fixnum file descriptor


Sets the undelying stream of the instance.
`
}

// type usocketInitCaller bool

// func (caller usocketInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
// 	obj := s.Get("self").(*flavors.Instance)
// 	if 0 < len(args) {
// 		args = args[0].(slip.List)
// 	}
// 	obj.Any = nil // TBD net.Conn
// 	network := "tcp"
// 	var (
// 		address string
// 		timeout time.Duration
// 	)
// 	for i := 0; i < len(args); i += 2 {
// 		key, _ := args[i].(slip.Symbol)
// 		k := string(key)
// 		switch {
// 		case strings.EqualFold(":network", k):
// 			network = getStrArg(args[i+1], k)
// 		case strings.EqualFold(":address", k):
// 			address = getStrArg(args[i+1], k)
// 		case strings.EqualFold(":timeout", k):
// 			timeout = getDurationArg(args[i+1], k)
// 		case strings.EqualFold(":file", k):
// 			if fs, ok := args[i+1].(*slip.FileStream); ok {
// 				if c, err := net.FileConn((*os.File)(fs)); err == nil {
// 					obj.Any = c
// 				} else {
// 					panic(err)
// 				}
// 			} else {
// 				slip.PanicType(":file", args[i+1], "file-stream")
// 			}
// 		case strings.EqualFold(":fd", k):
// 			fd := getIntArg(args[i+1], k)
// 			if f := os.NewFile(uintptr(fd), fmt.Sprintf("file-%d", fd)); f != nil {
// 				defer func() { _ = f.Close() }()
// 				var err error
// 				if obj.Any, err = net.FileConn(f); err != nil {
// 					panic(err)
// 				}
// 			} else {
// 				slip.NewPanic("%d is not a valid file descriptor", fd)
// 			}
// 		}
// 	}
// 	// TBD what about listening?
// 	if obj.Any == nil && 0 < len(address) {
// 		var err error
// 		if 0 < timeout {
// 			obj.Any, err = net.DialTimeout(network, address, timeout)
// 		} else {
// 			obj.Any, err = net.Dial(network, address)
// 		}
// 		if err != nil {
// 			panic(err)
// 		}
// 	}
// 	return nil
// }

// func (caller usocketInitCaller) Docs() string {
// 	return `__:init__ &key _network_ _address_ _timeout_ _file_ _fd_
//    _:network_ [string] must be one of "tcp", "tcp4", "tcp6", "udp", "udp4", "udp6", "ip", "ip4", "ip6", "unix",
// "unixgram" or "unixpacket".
//    _:address_ [string] the format should be <host>:<port>.
//    _:timeout_ [real] the timeout in seconds.
//    _:file_ [file-stream] a file-stream to use as socket.
//    _:fd_ [fixnum] a file descriptor.

// Initializes an instance. If an _address_ is provided then an attempt will be
// made connect to that address on the network provided or the default 'tcp'. The
// _timeout_ also applies if provided. If a _file_ or _fd_ is provided a socket
// is created for the designated file.
// `
// }
