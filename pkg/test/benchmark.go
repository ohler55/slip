// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"math"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Benchmark{Function: slip.Function{Name: "benchmark", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "benchmark",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to benchmark.",
				},
				{Name: "&optional"},
				{
					Name: "duration",
					Type: "real",
					Text: "The number of seconds to limit the benchmark to. The default is 3.0 seconds.",
				},
				{
					Name: "iterations",
					Type: "fixnum",
					Text: "The number of iteration to make. The default is _nil_ for no limit.",
				},
			},
			Return: "double-float",
			Text: `__benchmark__ runs the _functions_ until either _iterations_ or _duration_ is reached.
The timer per iteration is returned.`,
			Examples: []string{
				`(benchmark (lambda () (* 1 2 3 4 5 6))) => 0.000001`,
			},
		}, &Pkg)
}

// Benchmark represents the benchmark function.
type Benchmark struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Benchmark) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	d2 := depth + 1
	caller := cl.ResolveToCaller(s, args[0], d2)
	dur := time.Second * 3
	iter := int64(math.MaxInt64)
	if 1 < len(args) {
		if r, ok := args[1].(slip.Real); ok {
			dur = time.Duration(r.RealValue() * float64(time.Second))
		} else {
			slip.TypePanic(s, depth, "duration", args[1], "real")
		}
		if 2 < len(args) {
			if num, ok := args[2].(slip.Fixnum); ok && 0 < num {
				iter = int64(num)
			} else {
				slip.TypePanic(s, depth, "iterations", args[2], "fixnum")
			}
		}
	}
	start := time.Now()
	var cnt int64
	for time.Since(start) < dur && cnt < iter {
		_ = caller.Call(s, nil, d2)
		cnt++
	}
	elapsed := time.Since(start)

	return slip.DoubleFloat(float64(elapsed) / float64(time.Second) / float64(cnt))
}
