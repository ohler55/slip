// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"reflect"
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Select{Function: slip.Function{Name: "select", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "select",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "clause*",
					Type: "list",
					Text: `Each clause _(channel var form*)_ takes the form of a channel, a symbol, and
zero of more forms to be evaluate if the _channel_ pops a value.`,
				},
			},
			Return: "object",
			Text: `__select__ waits for the first channel to pop a value from and then
evaluates each form in the _clause_ for that channel.`,
			Examples: []string{
				"(let ((c1 (make-channel 10)) (c2 (time.after 0.1)))",
				" (select (c1 x (+ 1 x)) (c2 x x))) => @2024-11-25T20:36:28Z",
			},
		}, &Pkg)
}

// Select represents the select function.
type Select struct {
	slip.Function
}

const (
	maxTimeChan = 2
	maxSlipChan = 8
)

// Call the function with the arguments provided.
func (f *Select) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	var (
		clauses [maxSlipChan + maxTimeChan]slip.List
		sc      [maxSlipChan]Channel
		tc      [maxTimeChan]TimeChannel
		v       any
		ti      int
		ci      int
	)
	d2 := depth + 1
	if f.prepClauses(s, args, d2) {
		return f.reflectClauses(s, args, d2)
	}
	for _, a := range args {
		clause := a.(slip.List)
		switch c1 := clause[0].(type) {
		case Channel:
			sc[ci] = c1
			clauses[ci] = clause
			ci++
		case TimeChannel:
			tc[ti] = c1
			clauses[maxSlipChan+ti] = clause
			ti++
		}
		if 1 < len(clause) {
			if _, ok := clause[1].(slip.Symbol); !ok {
				slip.TypePanic(s, depth, "clause[1]", clause[1], "symbol")
			}
		}
	}
	// Note if the maxSlipChan or maxTimeChan values are changed the select
	// case must also be changed.
	select {
	case v = <-sc[0]:
		result = evalClause(s, clauses[0], v, d2)
	case v = <-sc[1]:
		result = evalClause(s, clauses[1], v, d2)
	case v = <-sc[2]:
		result = evalClause(s, clauses[2], v, d2)
	case v = <-sc[3]:
		result = evalClause(s, clauses[3], v, d2)
	case v = <-sc[4]:
		result = evalClause(s, clauses[4], v, d2)
	case v = <-sc[5]:
		result = evalClause(s, clauses[5], v, d2)
	case v = <-sc[6]:
		result = evalClause(s, clauses[6], v, d2)
	case v = <-sc[7]:
		result = evalClause(s, clauses[7], v, d2)
	case v = <-tc[0]:
		result = evalClause(s, clauses[8], v, d2)
	case v = <-tc[1]:
		result = evalClause(s, clauses[9], v, d2)
	}
	return
}

func (f *Select) prepClauses(s *slip.Scope, args slip.List, depth int) bool {
	var (
		ccnt int
		tcnt int
		refl bool
	)
	for _, a := range args {
		clause, ok := a.(slip.List)
		if !ok || len(clause) == 0 {
			slip.TypePanic(s, depth, "clause", a, "list")
		}
		clause[0] = slip.EvalArg(s, clause, 0, depth)
		switch clause[0].(type) {
		case Channel:
			ccnt++
		case TimeChannel:
			tcnt++
		default:
			refl = true
		}
	}
	return refl || maxTimeChan < tcnt || maxSlipChan < ccnt
}

func (f *Select) reflectClauses(s *slip.Scope, clauses slip.List, depth int) (result slip.Object) {
	cases := make([]reflect.SelectCase, len(clauses))
	for i, a := range clauses {
		clause := a.(slip.List)
		rcv := reflect.ValueOf(clause[0])
		if rcv.Kind() != reflect.Chan {
			slip.TypePanic(s, depth, "clause[0]", a, "channel")
		}
		cases[i] = reflect.SelectCase{
			Dir:  reflect.SelectRecv,
			Chan: rcv,
		}
	}
	chosen, recv, ok := reflect.Select(cases)
	if ok {
		result = evalClause(s, clauses[chosen].(slip.List), recv.Interface(), depth)
	}
	return
}

func evalClause(s *slip.Scope, clause slip.List, v any, depth int) (result slip.Object) {
	if len(clause) < 2 {
		return
	}
	ns := s.NewScope()
	var obj slip.Object

	switch tv := v.(type) {
	case nil:
		// remains nil
	case slip.Object:
		obj = tv
	case time.Time:
		obj = slip.Time(tv)
	}
	if sym, ok := clause[1].(slip.Symbol); ok {
		ns.UnsafeLet(sym, obj)
	}
	for i := 2; i < len(clause); i++ {
		result = slip.EvalArg(ns, clause, i, depth)
	}
	return
}
