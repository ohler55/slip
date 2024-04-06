// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"errors"
	"fmt"
	"io"
	"net"
	"sync/atomic"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	clientFlavor *flavors.Flavor
)

func init() {
	Pkg.Initialize(nil)
	_ = ClientFlavor()
}

type symVal struct {
	sym slip.Symbol
	val slip.Object
}

type client struct {
	wcon
	self    slip.Instance
	host    string
	port    int
	vars    []*symVal
	cnt     atomic.Int64
	results chan slip.Object
	changes chan slip.Object
	evalMap map[int]chan slip.Object
}

// ClientFlavor returns the watch-client flavor.
func ClientFlavor() *flavors.Flavor {
	if clientFlavor == nil {
		clientFlavor = flavors.DefFlavor("watch-client", map[string]slip.Object{}, nil,
			slip.List{
				slip.List{
					slip.Symbol(":init-keywords"),
					slip.Symbol(":host"),
					slip.Symbol(":port"),
					slip.Symbol(":vars"),
				},
				slip.List{
					slip.Symbol(":documentation"),
					slip.String(`A client that... TBD.`),
				},
			},
			&Pkg,
		)
		clientFlavor.DefMethod(":init", "", clientInitCaller{})
		clientFlavor.DefMethod(":shutdown", "", clientShutdownCaller{})
		clientFlavor.DefMethod(":eval", "", clientEvalCaller{})
		clientFlavor.DefMethod(":watch", "", clientWatchCaller{})
		clientFlavor.DefMethod(":forget", "", clientForgetCaller{})
		clientFlavor.DefMethod(":changed", "", clientChangedCaller{})
		// TBD
	}
	return clientFlavor
}

type clientInitCaller struct{}

func (caller clientInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	c := &client{
		self:    self,
		results: make(chan slip.Object, 100),
		changes: make(chan slip.Object, 100),
		evalMap: map[int]chan slip.Object{},
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":host")); has {
		if ss, ok := v.(slip.String); ok {
			c.host = string(ss)
		} else {
			slip.PanicType(":host", v, "string")
		}
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":port")); has {
		if num, ok := v.(slip.Fixnum); ok {
			c.port = int(num)
		} else {
			slip.PanicType(":port", v, "fixnum")
		}
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":vars")); has {
		if list, ok := v.(slip.List); ok {
			for _, v2 := range list {
				if sym, ok := v2.(slip.Symbol); ok {
					c.vars = append(c.vars, &symVal{sym: sym, val: slip.Unbound})
				} else {
					slip.PanicType(":vars", v2, "list of symbols")
				}
			}
		} else {
			slip.PanicType(":vars", v, "list of symbols")
		}
	}
	self.Any = c
	var err error
	if c.con, err = net.Dial("tcp", fmt.Sprintf("%s:%d", c.host, c.port)); err != nil {
		panic(err)
	}
	go c.resultLoop()
	go c.changeLoop()
	go c.listen(s)
	for _, v := range c.vars {
		req := slip.List{slip.Symbol("watch"), v.sym}
		msg := watchPrinter.Append(nil, req, 0)
		msg = append(msg, '\n')
		if _, err := c.con.Write(msg); err != nil {
			c.shutdown()
			return slip.NewError("%s", err)
		}
	}
	return nil
}

func (caller clientInitCaller) Docs() string {
	return `__:init__ &key _host_ _port_
   _:host_ [string] the host to connect to.
   _:port_ [fixnum] the port to connect to.
   _:vars_ [list] initial variables to watch.


Sets the initial values when _make-instance_ is called.
`
}

type clientShutdownCaller struct{}

func (caller clientShutdownCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(self, ":shutdown", len(args), "0")
	}
	c := self.Any.(*client)
	c.shutdown()

	return nil
}

func (caller clientShutdownCaller) Docs() string {
	return `__:shutdown__ => _nil_


Shuts down the client.
`
}

type clientEvalCaller struct{}

func (caller clientEvalCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	if 3 < len(args) {
		flavors.PanicMethodArgChoice(self, ":eval", len(args), "1 to 3")
	}
	c := self.Any.(*client)
	timeout := time.Second * 2
	if v, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":timeout")); has {
		if num, ok := v.(slip.Real); ok {
			timeout = time.Duration(num.RealValue() * float64(time.Second))
		} else {
			slip.PanicType(":timeout", v, "real")
		}
	}
	id := c.cnt.Add(1)
	req := slip.List{slip.Symbol("eval"), slip.Fixnum(id), args[0]}
	msg := watchPrinter.Append(nil, req, 0)
	msg = append(msg, '\n')
	rc := make(chan slip.Object)
	c.mu.Lock()
	c.evalMap[int(id)] = rc
	c.mu.Unlock()
	if _, err := c.con.Write(msg); err != nil {
		c.shutdown()
		return slip.NewError("%s", err)
	}
	timer := time.NewTimer(timeout)
	select {
	case result = <-rc:
		_ = timer.Stop()
	case <-timer.C:
		result = slip.NewError(":eval request timed out")
	}
	return
}

func (caller clientEvalCaller) Docs() string {
	return `__:eval__ _expr_ &key timeout => _object_
   _:expr_ [object] an expression to evaluate.
   _:timeout_ [real] the number of seconds to wait for a response before timing out.


Send an expression to the remote server and wait for a response or a timeout.
`
}

type clientWatchCaller struct{}

func (caller clientWatchCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		flavors.PanicMethodArgChoice(self, ":watch", len(args), "1")
	}
	c := self.Any.(*client)
	req := slip.List{slip.Symbol("watch"), args[0]}
	msg := watchPrinter.Append(nil, req, 0)
	msg = append(msg, '\n')
	if _, err := c.con.Write(msg); err != nil {
		c.shutdown()
		return slip.NewError("%s", err)
	}
	return nil
}

func (caller clientWatchCaller) Docs() string {
	return `__:watch__ _symbol_ _value_
   _:symbol_ [symbol] the symbol to watch.


Sends a request to watch a global variable to the watch-server.
`
}

type clientForgetCaller struct{}

func (caller clientForgetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		flavors.PanicMethodArgChoice(self, ":forget", len(args), "1")
	}
	c := self.Any.(*client)
	req := slip.List{slip.Symbol("forget"), args[0]}
	msg := watchPrinter.Append(nil, req, 0)
	msg = append(msg, '\n')
	if _, err := c.con.Write(msg); err != nil {
		c.shutdown()
		return slip.NewError("%s", err)
	}
	// TBD remove from c.vars

	return nil
}

func (caller clientForgetCaller) Docs() string {
	return `__:forget__ _symbol_
   _:symbol_ [symbol] the symbol to forget.


Sends a request to the watch-server to forget or stop watching a global variable.
`
}

type clientChangedCaller struct{}

func (caller clientChangedCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	if len(args) != 2 {
		flavors.PanicMethodArgChoice(self, ":changed", len(args), "2")
	}
	c := self.Any.(*client)
	for _, v := range c.vars {
		if args[0] == v.sym {
			v.val = args[1]
			return
		}
	}
	if sym, ok := args[0].(slip.Symbol); ok {
		c.vars = append(c.vars, &symVal{sym: sym, val: args[1]})
	}
	return
}

func (caller clientChangedCaller) Docs() string {
	return `__:changed__ _symbol_ _value_
   _:symbol_ [symbol] the symbol that changed.
   _:value_ [object] the new value for the symbol.


Responds to a change event received from the watch-server.
`
}

func (c *client) listen(s *slip.Scope) {
	c.active.Store(true)
	buf := make([]byte, 4096)
	for {
		cnt, err := c.con.Read(buf)
		if err != nil {
			if !errors.Is(err, io.EOF) && c.active.Load() {
				displayError("read error on %s: %s\n", c.self, err)
				c.shutdown()
			}
			break
		}
		c.expr = append(c.expr, buf[:cnt]...)
		for {
			obj := c.readExpr()
			if obj != nil {
				if serr, ok := obj.(slip.Error); ok {
					c.expr = c.expr[:0]
					c.results <- slip.List{nil, serr}
					continue
				}
				if list, ok := obj.(slip.List); ok && 2 <= len(list) {
					switch list[0] {
					case slip.Symbol("result"):
						c.results <- list[1:]
					case slip.Symbol("error"):
						c.results <- slip.List{nil, slip.NewError("%s", list[1])}
					case slip.Symbol("changed"):
						c.changes <- list[1:]
					}
				}
			}
			if obj == nil || len(c.expr) == 0 {
				break
			}
		}
	}
	c.active.Store(false)
}

func (c *client) shutdown() {
	if c.active.Load() {
		c.active.Store(false)
		close(c.results)
		close(c.changes)
		_ = c.con.Close()
	}
}

func (c *client) resultLoop() {
	for {
		obj := <-c.results
		if obj == nil {
			break
		}
		if result, ok := obj.(slip.List); ok && 2 <= len(result) {
			var id slip.Fixnum
			if id, ok = result[0].(slip.Fixnum); ok {
				var (
					rc  chan slip.Object
					has bool
				)
				c.mu.Lock()
				if rc, has = c.evalMap[int(id)]; has {
					delete(c.evalMap, int(id))
				}
				c.mu.Unlock()
				if has {
					rc <- result[1]
				}
			}
		}
	}
}

func (c *client) changeLoop() {
	scope := slip.NewScope()
	for {
		obj := <-c.changes
		if obj == nil {
			break
		}
		if change, ok := obj.(slip.List); ok && 2 <= len(change) {
			_ = c.self.Receive(scope, ":changed", change, 0)
		}
	}
}
