// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"time"

	"github.com/ohler55/slip"
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
			result = nil // TBD create something that will travel
		}
	}()
	switch to := p.op.(type) {
	case slip.Symbol:
		result = slip.CurrentPackage.JustGet(string(to))
	case *slip.Lambda:
		result = to.Call(s, slip.List{}, 0)
	default:
		// TBD
	}
	return
}
