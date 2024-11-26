// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
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
evaluates each form in the _clause_ for that channel. Clauses are limited to two or less
_time-channel_ and 10 _channel_.`,
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

// Call the function with the arguments provided.
func (f *Select) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 12)
	var (
		clauses [12]slip.List
		sc      [10]Channel
		tc      [2]TimeChannel
		v       any
	)

	// TBD set up channels and clauses from args
	// check types

	d2 := depth + 1
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
	case v = <-sc[8]:
		result = evalClause(s, clauses[8], v, d2)
	case v = <-sc[9]:
		result = evalClause(s, clauses[9], v, d2)
	case v = <-tc[0]:
		result = evalClause(s, clauses[10], v, d2)
	case v = <-tc[1]:
		result = evalClause(s, clauses[11], v, d2)
	}

	// TBD reserve slot for 2 timechannel and 10 slip.channels
	// use dummy channels for those not used

	// for _, a := range args {
	// 	clause, ok := a.(slip.List)
	// 	if !ok || len(clause) == 0 {
	// 		slip.PanicType("clause", a, "list")
	// 	}
	// 	if slip.EvalArg(s, clause, 0, d2) == nil {
	// 		continue
	// 	}
	// 	for i := 1; i < len(clause); i++ {
	// 		result = slip.EvalArg(s, clause, i, d2)
	// 	}
	// 	break
	// }
	return
}

func evalClause(s *slip.Scope, clause slip.List, v any, depth int) (result slip.Object) {
	if len(clause) < 2 {
		return
	}
	ns := s.NewScope()
	var obj slip.Object

	switch tv := v.(type) {
	case nil, slip.Object:
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
