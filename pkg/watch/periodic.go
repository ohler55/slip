// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"fmt"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

type periodic struct {
	id     string
	period time.Duration
	next   time.Time
	op     slip.Object // lambda or symbol
}

func (p *periodic) eval(s *slip.Scope) (result slip.Object) {
	defer func() {
		if rec := recover(); rec != nil {
			fmt.Printf("*** periodic fail - %s\n", rec)
			result = nil // TBD create something that will travel
		}
	}()
top:
	switch to := p.op.(type) {
	case slip.Symbol:
		result = slip.CurrentPackage.JustGet(string(to))
	case slip.List:
		p.op, _ = cl.ResolveToCaller(s, to, 0).(*slip.Lambda)
		goto top
	case *slip.Lambda:
		result = to.Call(s, slip.List{}, 0)
	default:
		// TBD
	}
	return
}
