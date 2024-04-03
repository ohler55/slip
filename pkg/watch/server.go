// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"fmt"
	"net"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	serverFlavor *flavors.Flavor
)

func init() {
	Pkg.Initialize(nil)
	serverFlavor = flavors.DefFlavor("watch-server", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(":port"),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A server that listens on a port for LISP expressions. TBD.`),
			},
		},
		&Pkg,
	)
	serverFlavor.Final = true
	serverFlavor.DefMethod(":init", "", serverInitCaller{})
	serverFlavor.DefMethod(":shutdown", "", serverShutdownCaller{})
	serverFlavor.DefMethod(":connections", "", serverConnectionsCaller{})

	// TBD serverFlavor.DefMethod(":listening", "", serverListeningCaller{})
	// or serverFlavor.DefMethod(":activep", "", serverActivepCaller{})
}

type server struct {
	port     int
	listener net.Listener
	active   atomic.Bool
	mu       sync.Mutex
	cons     map[string]*connection
}

type serverInitCaller struct{}

func (caller serverInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	serv := &server{
		cons: map[string]*connection{},
	}
	for i := 0; i < len(args); i += 2 {
		if slip.Symbol(":port") == args[i] {
			if num, ok := args[i+1].(slip.Fixnum); ok {
				serv.port = int(num)
			} else {
				slip.PanicType(":port", args[i+1], "fixnum")
			}
		}
	}
	self.Any = serv
	var err error
	if serv.listener, err = net.Listen("tcp", fmt.Sprintf(":%d", serv.port)); err != nil {
		panic(err)
	}
	slip.AddSetHook(strconv.Itoa(serv.port), serv.setHook)
	go serv.listen()

	return nil
}

func (caller serverInitCaller) Docs() string {
	return `__:init__ &key _port_
   _:port_ [fixnum] the port to listen for connections on.


Sets the initial values when _make-instance_ is called.
`
}

type serverShutdownCaller struct{}

func (caller serverShutdownCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(self, ":shutdown", len(args), "0")
	}
	serv := self.Any.(*server)
	slip.RemoveSetHook(strconv.Itoa(serv.port))
	_ = serv.listener.Close()

	serv.mu.Lock()
	for _, c := range serv.cons {
		c.serv = nil
		c.shutdown()
	}
	serv.mu.Unlock()

	return nil
}

func (caller serverShutdownCaller) Docs() string {
	return `__:shutdown__ => _nil_


Shuts down the server.
`
}

type serverConnectionsCaller struct{}

func (caller serverConnectionsCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(self, ":connections", len(args), "0")
	}
	// TBD
	return nil
}

func (caller serverConnectionsCaller) Docs() string {
	return `__:connections__ => _list_


Returns a list of information about the current connections. The information
is an association list.
`
}

func (serv *server) listen() {
	serv.active.Store(true)
	for {
		con, err := serv.listener.Accept()
		if err != nil {
			if !strings.Contains(err.Error(), "closed") {
				displayError("accept on port %d failed: %s\n", serv.port, err)
			}
			break
		}
		c := newConnection(con)
		c.serv = serv
		serv.mu.Lock()
		serv.cons[c.id] = c
		serv.mu.Unlock()

		go c.listen()
	}
	serv.active.Store(false)
}

func (serv *server) setHook(p *slip.Package, key string) {
	var cons []*connection
	serv.mu.Lock()
	for _, c := range serv.cons {
		if c.watching[key] {
			cons = append(cons, c)
		}
	}
	serv.mu.Unlock()
	if 0 < len(cons) {
		msg := slip.List{slip.Symbol("changed"), slip.Symbol(key), p.JustGet(key)}
		for _, c := range cons {
			c.sendMsg(msg)
		}
	}
}
