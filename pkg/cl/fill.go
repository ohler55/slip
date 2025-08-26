// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Fill{Function: slip.Function{Name: "fill", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "fill",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequemce",
					Text: "The sequence to replace values in.",
				},
				{
					Name: "item",
					Type: "object",
					Text: "The value to fill the sequence with.",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: `The index of the start of selection to fill.`,
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: `The index of the end (exclusive) of selection to fill.`,
				},
			},
			Return: "sequence",
			Text: `__fill__ returns the _sequence_ with _selection_ element set to _item_.
This is a destructive operation for all but strings.`,
			Examples: []string{
				"(setq lst '(a b c)",
				"(fill lst 'x :start 1 :end 2) => (a x c)",
				"lst => (a x c)",
			},
		}, &slip.CLPkg)
}

// Fill represents the fill function.
type Fill struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Fill) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 6)
	item := args[1]
	var start int
	end := math.MaxInt
	kargs := args[2:]
	if v, ok := slip.GetArgsKeyValue(kargs, slip.Symbol(":start")); ok {
		start = getFixnumArg(s, v, ":start", depth)
	}
	if v, ok := slip.GetArgsKeyValue(kargs, slip.Symbol(":end")); ok {
		end = getFixnumArg(s, v, ":end", depth)
	}
	result = args[0]
	switch seq := args[0].(type) {
	case slip.List:
		end = checkStartEnd(s, start, end, len(seq), depth)
		for i := start; i < end; i++ {
			seq[i] = item
		}
	case slip.String:
		c, ok := item.(slip.Character)
		if !ok {
			slip.TypePanic(s, depth, "item", item, "character")
		}
		ra := []rune(seq)
		end = checkStartEnd(s, start, end, len(ra), depth)
		for i := start; i < end; i++ {
			ra[i] = rune(c)
		}
		result = slip.String(ra)
	case slip.VectorLike:
		end = checkStartEnd(s, start, end, seq.Length(), depth)
		for i := start; i < end; i++ {
			seq.Set(item, i)
		}
	default:
		slip.TypePanic(s, depth, "sequence", seq, "sequence")
	}
	return
}

func checkStartEnd(s *slip.Scope, start, end, size, depth int) int {
	if size <= start {
		slip.ErrorPanic(s, depth, ":start %d is greater than the size of %d", start, size)
	}
	if end == math.MaxInt {
		end = size
	} else if size <= end {
		slip.ErrorPanic(s, depth, ":end %d is greater than the size of %d", end, size)
	}
	if end < start {
		slip.ErrorPanic(s, depth, ":end of %d is less than :start of %d", end, start)
	}
	return end
}
