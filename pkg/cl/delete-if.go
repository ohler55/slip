// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"math"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DeleteIf{Function: slip.Function{Name: "delete-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "delete-if",
			Args: []*slip.DocArg{
				{
					Name: "test",
					Type: "object",
					Text: "The test to apply to each elements of _sequence_.",
				},
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to search for _item_ in.",
				},
				{Name: "&key"},
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
					Name: "count",
					Type: "fixnum",
					Text: `The maximum number of element to remove. Default is _nil_ for unlimited.`,
				},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _sequence_ to return a key for comparison.`,
				},
			},
			Return: "object",
			Text:   `__delete-if__ returns a sequence with the matching elements removed.`,
			Examples: []string{
				"(delete-if 'numberp '((y . 1) (1 . 2) (z . 3)) :key 'car) => ((y . 1) (z. 3))",
			},
		}, &slip.CLPkg)
}

// DeleteIf represents the delete-if function.
type DeleteIf struct {
	slip.Function
}

type deleteIfInfo struct {
	start   int
	end     int
	count   int
	test    slip.Caller
	key     slip.Caller
	fromEnd bool
}

// Call the function with the arguments provided.
func (f *DeleteIf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 12)
	var di deleteIfInfo
	di.test = ResolveToCaller(s, args[0], depth)
	di.end = -1
	di.count = math.MaxInt
	for pos := 2; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			di.key = ResolveToCaller(s, args[pos+1], depth)
		case ":start":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				di.start = int(num)
			} else {
				slip.PanicType("start", args[pos+1], "fixnum")
			}
		case ":end":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				di.end = int(num)
			} else if args[pos+1] == nil {
				di.end = -1
			} else {
				slip.PanicType("end", args[pos+1], "fixnum")
			}
		case ":count":
			if num, ok := args[pos+1].(slip.Fixnum); ok {
				di.count = int(num)
			} else {
				slip.PanicType("count", args[pos+1], "fixnum")
			}
		case ":from-end":
			di.fromEnd = args[pos+1] != nil
		default:
			slip.PanicType("keyword", sym, ":key", ":test")
		}
	}
	switch ta := args[1].(type) {
	case nil:
		// nothing to delete-if
	case slip.List:
		result = di.inList(s, ta, depth)
	case slip.String:
		result = di.inString(s, ta, depth)
	case slip.Vector:
		result = slip.Vector(di.inList(s, slip.List(ta), depth))
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

func (di *deleteIfInfo) inList(s *slip.Scope, seq slip.List, depth int) (list slip.List) {
	if di.end < 0 || len(seq) < di.end {
		di.end = len(seq)
	}
	d2 := depth + 1
	var count int
	if di.fromEnd {
		for i := len(seq) - 1; 0 <= i; i-- {
			if i < di.start || di.end <= i || di.count <= count {
				list = append(list, seq[i])
				continue
			}
			key := seq[i]
			if di.key != nil {
				key = di.key.Call(s, slip.List{key}, d2)
			}
			if di.test.Call(s, slip.List{key}, d2) != nil {
				count++
				continue
			}
			list = append(list, seq[i])
		}
		// reverse list
		for i := len(list)/2 - 1; 0 <= i; i-- {
			list[i], list[len(list)-i-1] = list[len(list)-i-1], list[i]
		}
	} else {
		for i := 0; i < len(seq); i++ {
			if i < di.start || di.end <= i || di.count <= count {
				list = append(list, seq[i])
				continue
			}
			key := seq[i]
			if di.key != nil {
				key = di.key.Call(s, slip.List{key}, d2)
			}
			if di.test.Call(s, slip.List{key}, d2) != nil {
				count++
				continue
			}
			list = append(list, seq[i])
		}
	}
	return list
}

func (di *deleteIfInfo) inString(s *slip.Scope, seq slip.String, depth int) slip.Object {
	ra := []rune(seq)
	if di.end < 0 || len(seq) < di.end {
		di.end = len(seq)
	}
	d2 := depth + 1
	var (
		count int
		nra   []rune
		key   slip.Object
	)
	if di.fromEnd {
		for i := len(ra) - 1; 0 <= i; i-- {
			if i < di.start || di.end <= i || di.count <= count {
				nra = append(nra, ra[i])
				continue
			}
			key = slip.Character(ra[i])
			if di.key != nil {
				key = di.key.Call(s, slip.List{key}, d2)
			}
			if di.test.Call(s, slip.List{key}, d2) != nil {
				count++
				continue
			}
			nra = append(nra, ra[i])
		}
		// reverse nra
		for i := len(nra)/2 - 1; 0 <= i; i-- {
			nra[i], nra[len(nra)-i-1] = nra[len(nra)-i-1], nra[i]
		}
	} else {
		for i := 0; i < len(ra); i++ {
			if i < di.start || di.end <= i || di.count <= count {
				nra = append(nra, ra[i])
				continue
			}
			key = slip.Character(ra[i])
			if di.key != nil {
				key = di.key.Call(s, slip.List{key}, d2)
			}
			if di.test.Call(s, slip.List{key}, d2) != nil {
				count++
				continue
			}
			nra = append(nra, ra[i])
		}
	}
	return slip.String(nra)
}
