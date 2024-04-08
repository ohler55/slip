// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"errors"
	"io"
	"net"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
)

type connection struct {
	wcon
	id        string
	serv      *server
	watching  map[string]bool
	periodics map[string]*periodic

	reqs      chan slip.Object
	sendQueue chan slip.Object
	evalQueue chan *periodic
}

func newConnection(con net.Conn) *connection {
	c := connection{
		reqs:      make(chan slip.Object, 100),
		sendQueue: make(chan slip.Object, 100),
		evalQueue: make(chan *periodic, 100),
		watching:  map[string]bool{},
		periodics: map[string]*periodic{},
	}
	c.con = con
	if addr := c.con.RemoteAddr(); addr != nil {
		c.id = addr.String()
	} else {
		c.id = gi.NewUUID().String()
	}
	return &c
}

func (c *connection) listen() {
	c.active.Store(true)
	go c.methodLoop()
	go c.sendLoop()
	go c.evalLoop()
	go c.periodicLoop()

	buf := make([]byte, 4096)
	for {
		cnt, err := c.con.Read(buf)
		if err != nil {
			if !errors.Is(err, io.EOF) && c.active.Load() {
				displayError("read error, closing connection to %s: %s", c.id, err)
			}
			break
		}
		c.expr = append(c.expr, buf[:cnt]...)
		for {
			obj := c.readExpr()
			if obj != nil {
				if serr, ok := obj.(slip.Error); ok {
					c.expr = c.expr[:0]
					c.sendQueue <- slip.List{slip.Symbol("error"), slip.String(serr.Error() + "\n")}
					continue
				}
				c.reqs <- obj
			}
			if obj == nil || len(c.expr) == 0 {
				break
			}
		}
	}
	c.shutdown()
}

func (c *connection) shutdown() {
	if c.active.Load() {
		c.active.Store(false)
		close(c.reqs)
		close(c.sendQueue)
		close(c.evalQueue)
		_ = c.con.Close()
		if c.serv != nil {
			c.serv.mu.Lock()
			delete(c.serv.cons, c.id)
			c.serv.mu.Unlock()
		}
	}
}

func (c *connection) methodLoop() {
	scope := slip.NewScope()
	for {
		obj := <-c.reqs
		if obj == nil {
			break
		}
		req, ok := obj.(slip.List)
		if !ok || len(req) < 2 { // need verb and id of request
			continue
		}
		c.evalReq(scope, req)
	}
}

func (c *connection) sendLoop() {
	for {
		obj := <-c.sendQueue
		if obj == nil {
			break
		}
		_, err := c.con.Write(watchPrinter.Append(nil, obj, 0))
		if err != nil {
			displayError("write error: %s", err)
			c.shutdown()
			break
		}
	}
}

func (c *connection) periodicLoop() {
	rest := time.Millisecond * 100
	for c.active.Load() {
		now := time.Now()
		c.mu.Lock()
		for _, p := range c.periodics {
			if p.next.Before(now) {
				c.evalQueue <- p
				p.next = p.next.Add(p.period)
			}
		}
		c.mu.Unlock()
		time.Sleep(rest)
	}
}

func (c *connection) evalLoop() {
	scope := slip.NewScope()
	for {
		p := <-c.evalQueue
		if p == nil {
			break
		}
		val := p.eval(scope)
		c.sendQueue <- slip.List{slip.Symbol("changed"), slip.Symbol(p.id), val}
	}
}

func (c *connection) evalReq(scope *slip.Scope, req slip.List) {
	switch req[0] {
	case slip.Symbol("eval"):
		if 2 < len(req) {
			// TBD should be a safe eval
			reply := req[2].Eval(scope, 0)
			c.sendQueue <- slip.List{slip.Symbol("result"), req[1], reply}
		}
	case slip.Symbol("watch"):
		if sym, ok := req[1].(slip.Symbol); ok {
			c.mu.Lock()
			c.watching[string(sym)] = true
			c.mu.Unlock()
			if val, has := slip.CurrentPackage.Get(string(sym)); has {
				msg := slip.List{slip.Symbol("changed"), sym, val}
				c.sendMsg(msg)
			}
		}
	case slip.Symbol("periodic"): // (periodic id op secs)
		if sym, ok := req[1].(slip.Symbol); ok && 3 < len(req) {
			if r, ok2 := req[3].(slip.Real); ok2 {
				p := periodic{
					id:     string(sym),
					period: time.Duration(r.RealValue() * float64(time.Second)),
					op:     req[2],
					next:   time.Now(),
				}
				c.mu.Lock()
				c.periodics[string(sym)] = &p
				c.mu.Unlock()
			}
		}
	case slip.Symbol("forget"):
		if sym, ok := req[1].(slip.Symbol); ok {
			c.mu.Lock()
			delete(c.watching, string(sym))
			delete(c.periodics, string(sym))
			c.mu.Unlock()
		}
	case slip.Symbol("close"):
		c.shutdown()
	}
}

func (c *connection) sendMsg(msg slip.List) {
	defer func() {
		_ = recover() // ignore panics
	}()
	c.sendQueue <- msg
}
