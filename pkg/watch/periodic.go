// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
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
			if result, _ = rec.(*slip.Panic); result == nil {
				cond := slip.NewError("%s", rec).(slip.Instance)
				result = slip.WrapError(s, cond, p.id, nil)
			}
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
		result = slip.NewError("%s is not a valid operation for a periodic.", to)
	}
	return
}

func (p *periodic) details() slip.List {
	return slip.List{
		slip.List{slip.Symbol("id"), slip.Tail{Value: slip.Symbol(p.id)}},
		slip.List{slip.Symbol("period"), slip.Tail{Value: slip.DoubleFloat(float64(p.period) / float64(time.Second))}},
		slip.List{slip.Symbol("op"), slip.Tail{Value: p.op}},
	}
}
