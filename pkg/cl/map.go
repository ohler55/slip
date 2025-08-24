// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Map{Function: slip.Function{Name: "map", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "map",
			Args: []*slip.DocArg{
				{
					Name: "result-type",
					Type: "symbol",
					Text: "The return sequence type.",
				},
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to call for each entry in _sequences_.",
				},
				{Name: "&rest"},
				{
					Name: "sequences",
					Type: "sequence",
					Text: "The sequences to iterate over.",
				},
			},
			Return: "nil",
			Text: `__map__ calls _function_ for each entry in the _lists_ with
an argument from each list. The result is then converted to the _result-type_.`,
			Examples: []string{
				`(map 'vector #'cons "abc" '(1 2 3)) => #((#\a . 1) (#\b . 2) (#\c . 3))`,
			},
		}, &slip.CLPkg)
}

// Map represents the map function.
type Map struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Map) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, -1)
	var rt string
	switch ta := args[0].(type) {
	case nil:
		// valid, leave rt empty
	case slip.Symbol:
		rt = strings.ToLower(string(ta))
		switch rt {
		case "list", "vector", "string", "octets":
		default:
			slip.TypePanic(s, depth, "result-type", ta, "nil", "list", "string", "vector", "octets")
		}
	default:
		// TBD expand in the future to support (vector * 3)
		slip.TypePanic(s, depth, "result-type", ta, "nil", "list", "string", "vector", "octets")
	}
	fn := args[1]
	d2 := depth + 1
	caller := ResolveToCaller(s, fn, d2)
	seqs := make([]slip.List, len(args)-2)
	for i, a := range args[2:] {
		seqs[i] = slip.CoerceToList(a).(slip.List)
	}
	var rlist slip.List
	if 1 < len(seqs) {
		min := len(seqs[0])
		for _, seq := range seqs {
			if len(seq) < min {
				min = len(seq)
			}
		}
		rlist = make(slip.List, min)
		ca := make(slip.List, len(seqs))
		for n := 0; n < min; n++ {
			for i, seq := range seqs {
				ca[i] = seq[n]
			}
			rlist[n] = caller.Call(s, ca, d2)
		}
	} else {
		// The most common case.
		for _, v := range seqs[0] {
			rlist = append(rlist, caller.Call(s, slip.List{v}, d2))
		}
	}
	switch rt {
	case "":
		// leave result as nil
	case "list":
		result = rlist
	case "string":
		result = slip.CoerceToString(rlist)
	case "vector":
		result = slip.CoerceToVector(rlist)
	case "octets":
		result = slip.CoerceToOctets(rlist)
	}
	return
}
