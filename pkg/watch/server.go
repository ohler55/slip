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

// ServerFlavor returns the server flavor.
func ServerFlavor() *flavors.Flavor {
	serverFlavor = flavors.DefFlavor("watch-server", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(":port"),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A server that listens on a port for LISP expressions.
The s-expressions must be a list with the first element a symbol. Depending on the
request type results are passed back immediately or changes are send back on change
or periodically. Additional details are included in the _*watch*_ package description.`),
			},
		},
		&Pkg,
	)
	serverFlavor.Final = true
	serverFlavor.DefMethod(":init", "", serverInitCaller{})
	serverFlavor.DefMethod(":shutdown", "", serverShutdownCaller{})
	serverFlavor.DefMethod(":connections", "", serverConnectionsCaller{})
	serverFlavor.DefMethod(":activep", "", serverActivepCaller{})

	return serverFlavor
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
	started := make(chan bool, 1)
	go serv.listen(started)
	<-started

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
		c.shutdown(false)
	}
	serv.cons = nil
	serv.mu.Unlock()

	return nil
}

func (caller serverShutdownCaller) Docs() string {
	return `__:shutdown__ => _nil_


Shuts down the server.
`
}

type serverActivepCaller struct{}

func (caller serverActivepCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(self, ":activep", len(args), "0")
	}
	serv := self.Any.(*server)
	if serv.active.Load() {
		return slip.True
	}
	return nil
}

func (caller serverActivepCaller) Docs() string {
	return `__:activep__ => _boolean_


Returns _t_ if the server is active.
`
}

type serverConnectionsCaller struct{}

func (caller serverConnectionsCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(self, ":connections", len(args), "0")
	}
	serv := self.Any.(*server)
	var cons slip.List
	serv.mu.Lock()
	for _, c := range serv.cons {
		cons = append(cons, c.details())
	}
	serv.mu.Unlock()

	return cons
}

func (caller serverConnectionsCaller) Docs() string {
	return `__:connections__ => _list_


Returns a list of information about the current connections. The information
is an association list.
`
}

func (serv *server) listen(started chan bool) {
	serv.active.Store(true)
	started <- true
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
		if serv.cons == nil {
			serv.mu.Unlock()
			break
		}
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
