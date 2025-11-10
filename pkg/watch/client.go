// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"errors"
	"io"
	"net"
	"strconv"
	"strings"
	"sync/atomic"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	clientFlavor *flavors.Flavor
)

type symVal struct {
	sym slip.Symbol
	val slip.Object
}

type client struct {
	wcon
	self    *flavors.Instance
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
					slip.Symbol(":watch"),
					slip.Symbol(":periodics"),
				},
				slip.List{
					slip.Symbol(":documentation"),
					slip.String(`A basic watch client that can be used to request evaluations on a _watch-server_.
Requests can be made to evaluate an s-expression on the remote server, watch a variable, or set up periodic
evaluations that will return the results to the client.`),
				},
			},
			&Pkg,
		)
		clientFlavor.DefMethod(":init", "", clientInitCaller{})
		clientFlavor.DefMethod(":shutdown", "", clientShutdownCaller{})
		clientFlavor.DefMethod(":activep", "", clientActivepCaller{})
		clientFlavor.DefMethod(":eval", "", clientEvalCaller{})
		clientFlavor.DefMethod(":watch", "", clientWatchCaller{})
		clientFlavor.DefMethod(":forget", "", clientForgetCaller{})
		clientFlavor.DefMethod(":changed", "", clientChangedCaller{})
		clientFlavor.DefMethod(":periodic", "", clientPeriodicCaller{})
	}
	return clientFlavor
}

type clientInitCaller struct{}

func (caller clientInitCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
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
			slip.TypePanic(s, depth, ":host", v, "string")
		}
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":port")); has {
		if num, ok := v.(slip.Fixnum); ok {
			c.port = int(num)
		} else {
			slip.TypePanic(s, depth, ":port", v, "fixnum")
		}
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":watch")); has {
		if list, ok := v.(slip.List); ok {
			for _, v2 := range list {
				if sym, ok := v2.(slip.Symbol); ok {
					c.vars = append(c.vars, &symVal{sym: sym, val: slip.Unbound})
				} else {
					slip.TypePanic(s, depth, ":watch", v2, "list of symbols")
				}
			}
		} else {
			slip.TypePanic(s, depth, ":watch", v, "list of symbols")
		}
	}
	self.Any = c
	var err error
	if c.con, err = net.Dial("tcp", net.JoinHostPort(c.host, strconv.Itoa(c.port))); err != nil {
		panic(err)
	}
	go c.resultLoop()
	go c.changeLoop()
	go c.listen(s)
	for _, v := range c.vars {
		req := slip.List{slip.Symbol("watch"), v.sym}
		if se := c.writeMsg(req); se != nil {
			panic(se)
		}
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":periodics")); has {
		if list, ok := v.(slip.List); ok {
			for _, v2 := range list {
				if pargs, ok2 := v2.(slip.List); ok2 {
					id := c.addPeriodic(s, self, pargs, depth)
					c.vars = append(c.vars, &symVal{sym: id, val: slip.Unbound})
				} else {
					slip.TypePanic(s, depth, ":periodics", v, "list of lists")
				}
			}
		} else {
			slip.TypePanic(s, depth, ":periodics", v, "list of lists")
		}
	}
	return nil
}

func (caller clientInitCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: "Sets the initial value when _make-instance_ is called.",
		Args: []*slip.DocArg{
			{Name: "&key"},
			{
				Name: ":host",
				Type: "string",
				Text: `The watch host to connect to.`,
			},
			{
				Name: ":port",
				Type: "fixnum",
				Text: " The watch port to connect to.",
			},
			{
				Name: ":vars",
				Type: "list",
				Text: `Initial variables to watch.`,
			},
		},
	}
}

type clientShutdownCaller struct{}

func (caller clientShutdownCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, self, ":shutdown", len(args), "0")
	}
	c := self.Any.(*client)
	c.shutdown()

	return nil
}

func (caller clientShutdownCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":shutdown",
		Text: "Shuts down the client.",
	}
}

type clientActivepCaller struct{}

func (caller clientActivepCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, self, ":activep", len(args), "0")
	}
	c := self.Any.(*client)
	if c.active.Load() {
		return slip.True
	}
	return nil
}

func (caller clientActivepCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":activep",
		Text: "Returns true if the client is active.",
	}
}

type clientEvalCaller struct{}

func (caller clientEvalCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	if len(args) < 1 || 3 < len(args) {
		slip.MethodArgChoicePanic(s, depth, self, ":eval", len(args), "1 to 3")
	}
	c := self.Any.(*client)
	timeout := time.Second * 2
	if v, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":timeout")); has {
		if num, ok := v.(slip.Real); ok {
			timeout = time.Duration(num.RealValue() * float64(time.Second))
		} else {
			slip.TypePanic(s, depth, ":timeout", v, "real")
		}
	}
	id := c.cnt.Add(1)
	req := slip.List{slip.Symbol("eval"), slip.Fixnum(id), args[0]}
	rc := make(chan slip.Object)
	c.mu.Lock()
	c.evalMap[int(id)] = rc
	c.mu.Unlock()

	if result = c.writeMsg(req); result == nil {
		timer := time.NewTimer(timeout)
		select {
		case result = <-rc:
			_ = timer.Stop()
		case <-timer.C:
			result = slip.ErrorNew(s, depth, ":eval request timed out")
		}
	}
	return
}

func (caller clientEvalCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":eval",
		Text: "Send an expression to the remote server and wait for a response or a timeout.",
		Args: []*slip.DocArg{
			{
				Name: "expr",
				Type: "object",
				Text: `An expression to evaluate.`,
			},
			{Name: "&key"},
			{
				Name: ":timeout",
				Type: "real",
				Text: "The number of seconds to wait for a response before timing out.",
			},
		},
		Return: "object",
	}
}

type clientWatchCaller struct{}

func (caller clientWatchCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		slip.MethodArgChoicePanic(s, depth, self, ":watch", len(args), "1")
	}
	c := self.Any.(*client)
	req := slip.List{slip.Symbol("watch"), args[0]}
	return c.writeMsg(req)
}

func (caller clientWatchCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":watch",
		Text: "Sends a request to watch a global variable to the watch-server.",
		Args: []*slip.DocArg{
			{
				Name: "symbol",
				Type: "symbol",
				Text: `The symbol to watch.`,
			},
		},
		Return: "object",
	}
}

type clientForgetCaller struct{}

func (caller clientForgetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		slip.MethodArgChoicePanic(s, depth, self, ":forget", len(args), "1")
	}
	c := self.Any.(*client)
	for i, sv := range c.vars {
		if sv.sym == args[0] {
			c.vars = append(c.vars[:i], c.vars[i+1:]...)
			break
		}
	}
	req := slip.List{slip.Symbol("forget"), args[0]}

	return c.writeMsg(req)
}

func (caller clientForgetCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":forget",
		Text: "Sends a request to the watch-server to forget or stop watching a global variable.",
		Args: []*slip.DocArg{
			{
				Name: "symbol",
				Type: "symbol",
				Text: `The symbol to forget.`,
			},
		},
		Return: "object",
	}
}

type clientChangedCaller struct{}

func (caller clientChangedCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if len(args) != 2 {
		slip.MethodArgChoicePanic(s, depth, self, ":changed", len(args), "2")
	}
	c := self.Any.(*client)
	for _, v := range c.vars {
		if args[0] == v.sym {
			v.val = args[1]
			return nil
		}
	}
	if sym, ok := args[0].(slip.Symbol); ok {
		c.vars = append(c.vars, &symVal{sym: sym, val: args[1]})
	}
	return nil
}

func (caller clientChangedCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":changed",
		Text: "Responds to a change event received from the watch-server.",
		Args: []*slip.DocArg{
			{
				Name: "symbol",
				Type: "symbol",
				Text: `The symbol that changed.`,
			},
			{
				Name: "value",
				Type: "object",
				Text: `The new value for the symbol.`,
			},
		},
	}
}

type clientPeriodicCaller struct{}

func (caller clientPeriodicCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	c := self.Any.(*client)
	_ = c.addPeriodic(s, self, args, depth)

	return
}

func (caller clientPeriodicCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":periodic",
		Text: "Adds a periodic evaluator to the watch server.",
		Args: []*slip.DocArg{
			{
				Name: "id",
				Type: "symbol",
				Text: `The periodic evaluation identifier.`,
			},
			{
				Name: "period",
				Type: "real",
				Text: `The number of seconds to wait between evaluations.`,
			},
			{
				Name: "op",
				Type: "symbol|lambda",
				Text: `The symbol of a variable or a lambda to evaluate on the server.`,
			},
		},
	}
}

func (c *client) listen(s *slip.Scope) {
	c.active.Store(true)
	buf := make([]byte, 4096)
	for {
		cnt, err := c.con.Read(buf)
		if err != nil {
			if !errors.Is(err, io.EOF) && c.active.Load() {
				if !strings.Contains(err.Error(), "connection reset by peer") {
					displayError("read error on %s: %s\n", c.self, err)
				}
				c.shutdown()
			}
			break
		}
		c.expr = append(c.expr, buf[:cnt]...)
		for {
			obj := c.readExpr()
			if obj != nil {
				if cond, ok := obj.(slip.Instance); ok && cond.IsA("error") {
					c.expr = c.expr[:0]
					c.results <- slip.List{nil, cond}
					continue
				}
				if list, ok := obj.(slip.List); ok && 2 <= len(list) {
					switch list[0] {
					case slip.Symbol("result"):
						c.results <- list[1:]
					case slip.Symbol("error"):
						serr := formError(s, list)
						switch tid := list[1].(type) {
						case nil, slip.Fixnum:
							c.results <- slip.List{tid, serr}
						case slip.Symbol, slip.String:
							c.changes <- slip.List{tid, serr}
						}
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

func formError(s *slip.Scope, list slip.List) (serr slip.Object) {
	msg := list[3].(slip.String)
	class := list[2].(slip.Symbol)

	if c := slip.FindClass(string(class)); c != nil && c.Metaclass() == slip.Symbol("condition-class") {
		obj := c.MakeInstance()
		obj.Init(slip.NewScope(), slip.List{slip.Symbol(":message"), msg}, 0)
		serr = obj
	} else {
		slip.ErrorPanic(s, 0, "%s does not designate a condition class.", class)
	}
	return
}

func (c *client) shutdown() {
	if c.active.Load() {
		c.active.Store(false)
		_ = c.con.Close()
		close(c.results)
		close(c.changes)
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

func (c *client) addPeriodic(s *slip.Scope, self *flavors.Instance, args slip.List, depth int) (id slip.Symbol) {
	if len(args) != 3 {
		slip.MethodArgChoicePanic(s, depth, self, ":periodic", len(args), "3")
	}
	if sym, ok := args[0].(slip.Symbol); ok {
		id = sym
	} else {
		slip.TypePanic(s, depth, ":id", args[0], "symbol")
	}
	op := args[2]
	if lam, ok := op.(*slip.Lambda); ok {
		op = append(slip.List{slip.Symbol("lambda"), slip.List{}}, lam.Forms...)
	}
	req := slip.List{slip.Symbol("periodic"), id, args[1], op}
	if se := c.writeMsg(req); se != nil {
		panic(se)
	}
	return
}

func (c *client) writeMsg(x slip.List) slip.Object {
	msg := watchPrinter.Append(nil, x, 0)
	msg = append(msg, '\n')
	if cnt, err := c.con.Write(msg); err != nil || cnt == 0 {
		c.shutdown()
		return slip.ErrorNew(slip.NewScope(), 0, "%s", err)
	}
	return nil
}
