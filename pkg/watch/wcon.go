// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"bytes"
	"math"
	"net"
	"sync"
	"sync/atomic"

	"github.com/ohler55/slip"
)

var watchPrinter = slip.Printer{
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

type wcon struct {
	con    net.Conn
	expr   []byte
	active atomic.Bool
	mu     sync.Mutex
}

func (c *wcon) readExpr() (obj slip.Object) {
	defer func() {
		if rec := recover(); rec != nil {
			if _, ok := rec.(*slip.PartialPanic); !ok {
				if serr, ok := rec.(slip.Error); ok {
					obj = serr
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
