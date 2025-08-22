// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Merge{Function: slip.Function{Name: "merge", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "merge",
			Args: []*slip.DocArg{
				{
					Name: "result-type",
					Type: "symbol",
					Text: "The return sequence type. If _nil_ the default _list_ is assumed.",
				},
				{
					Name: "sequence-1",
					Type: "sequence",
					Text: `The first _sequence_ for the merge.`,
				},
				{
					Name: "sequence-2",
					Type: "sequence",
					Text: `The second _sequence_ for the merge.`,
				},
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns _t_ to
indicate the first argument is less than the ssecond. The default is __<__.`,
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _list_ to return a key for comparison.`,
				},
			},
			Return: "sequence",
			Text:   `__merge__ returns a list of all the unique elements of both list. Order is arbitrary.`,
			Examples: []string{
				`(merge '(1 2 3) '(0 3 4 5)) => (0 1 2 3 3 4 5)`,
			},
		}, &slip.CLPkg)
}

// Merge represents the merge function.
type Merge struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Merge) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 4, 6)
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
		slip.TypePanic(s, depth, "result-type", ta, "nil", "list", "string", "vector", "octets")
	}
	seq1 := slip.CoerceToList(args[1]).(slip.List)
	seq2 := slip.CoerceToList(args[2]).(slip.List)
	d2 := depth + 1
	var (
		keyFunc   slip.Caller
		predicate slip.Caller
	)

	if args[3] != nil {
		predicate = ResolveToCaller(s, args[3], d2)
	}
	if v, has := slip.GetArgsKeyValue(args[4:], slip.Symbol(":key")); has {
		keyFunc = ResolveToCaller(s, v, d2)
	}
	rlist := make(slip.List, 0, len(seq1)+len(seq2))
	for {
		if len(seq1) == 0 {
			rlist = append(rlist, seq2...)
			break
		}
		if len(seq2) == 0 {
			rlist = append(rlist, seq1...)
			break
		}
		k1 := seq1[0]
		k2 := seq2[0]
		if keyFunc != nil {
			k1 = keyFunc.Call(s, slip.List{k1}, d2)
			k2 = keyFunc.Call(s, slip.List{k2}, d2)
		}
		var less bool
		if predicate == nil {
			less = sortLess(k1, k2)
		} else {
			less = predicate.Call(s, slip.List{k1, k2}, d2) != nil
		}
		if less {
			rlist = append(rlist, seq1[0])
			seq1 = seq1[1:]
		} else {
			rlist = append(rlist, seq2[0])
			seq2 = seq2[1:]
		}
	}
	switch rt {
	case "", "list":
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
