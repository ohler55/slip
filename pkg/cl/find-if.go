// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FindIf{Function: slip.Function{Name: "find-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "find-if",
			Args: []*slip.DocArg{
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: "The function to apply to each element of _sequence_.",
				},
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to search.",
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
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _sequence_ to return a key for comparison.`,
				},
			},
			Return: "object",
			Text:   `__find-if__ returns the first element that satisfies _test_ or _nil_ if there is no match.`,
			Examples: []string{
				"(find-if 'x '((y . 1) (y. 2) (z . 3)) :key 'car) => (x . 1)",
			},
		}, &slip.CLPkg)
}

// FindIf represents the find-if function.
type FindIf struct {
	slip.Function
}

type findIfInfo struct {
	start     int
	end       int
	predicate slip.Caller
	key       slip.Caller
	fromEnd   bool
}

// Call the function with the arguments provided.
func (f *FindIf) Call(s *slip.Scope, args slip.List, depth int) (found slip.Object) {
	slip.ArgCountCheck(f, args, 2, 10)
	var fi findIfInfo
	fi.predicate = ResolveToCaller(s, args[0], depth)
	fi.end = -1
	for pos := 2; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			fi.key = ResolveToCaller(s, args[pos+1], depth)
		case ":start":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				fi.start = int(num)
			} else {
				slip.PanicType("start", args[pos+1], "fixnum")
			}
		case ":end":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				fi.end = int(num)
			} else if args[pos+1] == nil {
				fi.end = -1
			} else {
				slip.PanicType("end", args[pos+1], "fixnum")
			}
		case ":from-end":
			fi.fromEnd = args[pos+1] != nil
		default:
			slip.PanicType("keyword", sym, ":key", ":test")
		}
	}
	switch ta := args[1].(type) {
	case nil:
		// nothing found
	case slip.List:
		found = fi.inList(s, ta, depth)
	case slip.String:
		found = fi.inString(s, ta, depth)
	case slip.Vector:
		found = fi.inList(s, slip.List(ta), depth)
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

func (fi *findIfInfo) inList(s *slip.Scope, seq slip.List, depth int) slip.Object {
	if len(seq) <= fi.start {
		return nil
	}
	if 0 <= fi.end && fi.end < len(seq) {
		seq = seq[fi.start:fi.end]
	} else {
		seq = seq[fi.start:]
	}
	d2 := depth + 1
	if !fi.fromEnd {
		for _, element := range seq {
			key := element
			if fi.key != nil {
				key = fi.key.Call(s, slip.List{key}, d2)
			}
			if fi.predicate.Call(s, slip.List{key}, d2) != nil {
				return element
			}
		}
	}
	for i := len(seq) - 1; 0 <= i; i-- {
		key := seq[i]
		if fi.key != nil {
			key = fi.key.Call(s, slip.List{key}, d2)
		}
		if fi.predicate.Call(s, slip.List{key}, d2) != nil {
			return seq[i]
		}
	}
	return nil
}

func (fi *findIfInfo) inString(s *slip.Scope, seq slip.String, depth int) (found slip.Object) {
	ra := []rune(seq)
	if len(ra) <= fi.start {
		return nil
	}
	if 0 <= fi.end && fi.end < len(ra) {
		ra = ra[fi.start:fi.end]
	} else {
		ra = ra[fi.start:]
	}
	d2 := depth + 1
	var key slip.Object
	if !fi.fromEnd {
		for _, element := range ra {
			key = slip.Character(element)
			if fi.key != nil {
				key = fi.key.Call(s, slip.List{key}, d2)
			}
			if fi.predicate.Call(s, slip.List{key}, d2) != nil {
				return slip.Character(element)
			}
		}
	}
	for i := len(ra) - 1; 0 <= i; i-- {
		key = slip.Character(ra[i])
		if fi.key != nil {
			key = fi.key.Call(s, slip.List{key}, d2)
		}
		if fi.predicate.Call(s, slip.List{key}, d2) != nil {
			return slip.Character(ra[i])
		}
	}
	return nil
}
