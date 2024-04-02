// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"math"
	"net"
	"sync"
	"sync/atomic"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
)

type connection struct {
	id        string
	con       net.Conn
	serv      *server
	expr      []byte
	reqs      chan slip.Object
	sendQueue chan slip.Object
	active    atomic.Bool
	mu        sync.Mutex
}

func newConnection(con net.Conn) *connection {
	c := connection{
		con:       con,
		reqs:      make(chan slip.Object, 100),
		sendQueue: make(chan slip.Object, 100),
	}
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
	buf := make([]byte, 4096)
	for {
		cnt, err := c.con.Read(buf)
		if err != nil {
			if !errors.Is(err, io.EOF) {
				fmt.Printf("*** read err: %s\n", err)
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

func (c *connection) readExpr() (obj slip.Object) {
	defer func() {
		if rec := recover(); rec != nil {
			if _, ok := rec.(*slip.PartialPanic); !ok {
				if serr, ok := rec.(slip.Error); ok {
					obj = serr
				} else {
					obj = slip.NewError("%s", rec)
				}
			}
		}
	}()
	code, pos := slip.ReadOne(c.expr)
	if 0 < len(code) {
		obj = code[0]
	}
	after := len(c.expr) - pos
	if after <= 0 {
		c.expr = c.expr[:0]
		return
	}
	copy(c.expr, c.expr[pos:])
	c.expr = bytes.TrimSpace(c.expr[:after])

	return
}

func (c *connection) shutdown() {
	if c.active.Load() {
		c.active.Store(false)
		close(c.reqs)
		close(c.sendQueue)
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
	p := slip.Printer{
		Base:        10,
		Case:        slip.Symbol(":downcase"),
		Escape:      true,
		Length:      math.MaxInt,
		Level:       math.MaxInt,
		Lines:       math.MaxInt,
		Prec:        -1,
		Pretty:      true,
		Readably:    true,
		RightMargin: 120,
	}
	for {
		obj := <-c.sendQueue
		if obj == nil {
			break
		}
		_, err := c.con.Write(p.Append(nil, obj, 0))
		if err != nil {
			fmt.Printf("*** write error: %s\n", err)
			c.shutdown()
			break
		}
	}
}

func (c *connection) evalReq(scope *slip.Scope, req slip.List) {
	var reply slip.Object

	switch req[0] {
	case slip.Symbol("eval"):
		if 2 < len(req) {
			reply = req[2].Eval(scope, 0)
		}
	case slip.Symbol("match"):
		// TBD
	case slip.Symbol("periodic"):
		// TBD
	case slip.Symbol("forget"):
		// TBD
	case slip.Symbol("close"):
		// TBD
	}
	c.sendQueue <- slip.List{req[1], reply}
}
