// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"math"
	"net"

	"github.com/ohler55/slip"
)

type connection struct {
	con       net.Conn
	serv      *server
	expr      []byte
	reqs      chan slip.Object
	sendQueue chan slip.Object
}

func (c *connection) listen() {
	c.reqs = make(chan slip.Object, 100)
	c.sendQueue = make(chan slip.Object, 100)

	go c.methodLoop()
	go c.sendLoop()

	fmt.Printf("*** local: %s  remote: %s\n", c.con.LocalAddr(), c.con.RemoteAddr())
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
				c.reqs <- obj
			}
			if obj == nil || len(c.expr) == 0 {
				break
			}
		}
	}
}

func (c *connection) readExpr() (obj slip.Object) {
	defer func() {
		if rec := recover(); rec != nil {
			if _, ok := rec.(*slip.PartialPanic); !ok {
				fmt.Printf("*** rec: %T %s\n", rec, rec)
				panic(rec)
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
	c.reqs <- nil
	c.sendQueue <- nil
	_ = c.con.Close()
}

func (c *connection) methodLoop() {
	scope := slip.NewScope()
	for {
		obj := <-c.reqs
		if obj == nil {
			return
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
			return
		}
		_, err := c.con.Write(p.Append(nil, obj, 0))
		if err != nil {
			fmt.Printf("*** write error: %s\n", err)
			c.shutdown()
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
