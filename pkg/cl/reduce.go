// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Reduce{Function: slip.Function{Name: "reduce", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "reduce",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments to apply to each set of
consecutive elements. It must return _t_ is the first argument is less than the second
for the given context.`,
				},
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to reduce.",
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _sequence_ to return a key for to the _function_.`,
				},
				{
					Name: "from-end",
					Type: "boolean",
					Text: `If true the search is in reverse or or from the end to the start.`,
				},
				{
					Name: "start",
					Type: "fixnum",
					Text: `The index to the first element in the sequence to check. Defaults to 0`,
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: `The index to the last element in the sequence to check. Defaults to nil,
the length of the _sequence_.`,
				},
				{
					Name: "initial-value",
					Type: "object",
					Text: `An initial value for the reduce sequence.`,
				},
			},
			Return: "object",
			Text: `__reduce__ returns the result of applying _function_ to each element from _start_
to _end_ with the first argument being the result of the previous invocation of the _function_.`,
			Examples: []string{
				"(reduce #'+ '(1 2 3 4) :initial-value -2) => 8",
			},
		}, &slip.CLPkg)
}

// Reduce represents the reduce function.
type Reduce struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Reduce) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 12)
	d2 := depth + 1
	caller := ResolveToCaller(s, args[0], d2)
	list := slip.CoerceToList(args[1]).(slip.List)
	args = args[2:]
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":end")); has && v != nil {
		if num, ok := v.(slip.Fixnum); ok && 0 <= num && int(num) <= len(list) {
			list = list[:int(num)]
		} else {
			slip.TypePanic(s, depth, ":end", v, fmt.Sprintf("fixnum between 0 and %d", len(list)))
		}
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":start")); has {
		if num, ok := v.(slip.Fixnum); ok && 0 <= num && int(num) < len(list) {
			list = list[int(num):]
		} else {
			slip.TypePanic(s, depth, ":start", v, fmt.Sprintf("fixnum between 0 and %d", len(list)))
		}
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":key")); has {
		keyFunc := ResolveToCaller(s, v, d2)
		for i, v2 := range list {
			list[i] = keyFunc.Call(s, slip.List{v2}, d2)
		}
	}
	var hasInit bool
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":initial-value")); has {
		result = v
		hasInit = true
	}
	if 0 < len(list) {
		if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":from-end")); has && v != nil {
			if !hasInit {
				result = list[len(list)-1]
				list = list[:len(list)-1]
			}
			for i := len(list) - 1; 0 <= i; i-- {
				result = caller.Call(s, slip.List{list[i], result}, d2)
			}
		} else {
			if !hasInit {
				result = list[0]
				list = list[1:]
			}
			for _, v := range list {
				result = caller.Call(s, slip.List{result, v}, d2)
			}
		}
	}
	return
}
