// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Find{Function: slip.Function{Name: "find", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "find",
			Args: []*slip.DocArg{
				{
					Name: "item",
					Type: "object",
					Text: "The value to match against the elements of _sequence_.",
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
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns a boolean to
indicate a match. The default is _equal_`,
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
			Return: "cons|list",
			Text:   `__find__ returns the first element that satisfies _test_ or _nil_ if there is no match.`,
			Examples: []string{
				"(find 'x '((y . 1) (y. 2) (z . 3)) :key 'car) => (x . 1)",
			},
		}, &slip.CLPkg)
}

// Find represents the find function.
type Find struct {
	slip.Function
}

type findInfo struct {
	item    slip.Object
	start   int
	end     int
	test    slip.Caller
	key     slip.Caller
	fromEnd bool
}

// Call the function with the arguments provided.
func (f *Find) Call(s *slip.Scope, args slip.List, depth int) (found slip.Object) {
	slip.ArgCountCheck(f, args, 2, 12)
	var fi findInfo
	fi.item = args[0]
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
		case ":test":
			fi.test = ResolveToCaller(s, args[pos+1], depth)
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

func (fi *findInfo) inList(s *slip.Scope, seq slip.List, depth int) slip.Object {
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
			if fi.test == nil {
				if slip.ObjectEqual(fi.item, key) {
					return element
				}
			} else {
				if fi.test.Call(s, slip.List{fi.item, key}, d2) != nil {
					return element
				}
			}
		}
	}
	for i := len(seq) - 1; 0 <= i; i-- {
		key := seq[i]
		if fi.key != nil {
			key = fi.key.Call(s, slip.List{key}, d2)
		}
		if fi.test == nil {
			if slip.ObjectEqual(fi.item, key) {
				return seq[i]
			}
		} else {
			if fi.test.Call(s, slip.List{fi.item, key}, d2) != nil {
				return seq[i]
			}
		}
	}
	return nil
}

func (fi *findInfo) inString(s *slip.Scope, seq slip.String, depth int) (found slip.Object) {
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
			if fi.test == nil {
				if slip.ObjectEqual(fi.item, key) {
					return slip.Character(element)
				}
			} else {
				if fi.test.Call(s, slip.List{fi.item, key}, d2) != nil {
					return slip.Character(element)
				}
			}
		}
	}
	for i := len(ra) - 1; 0 <= i; i-- {
		key = slip.Character(ra[i])
		if fi.key != nil {
			key = fi.key.Call(s, slip.List{key}, d2)
		}
		if fi.test == nil {
			if slip.ObjectEqual(fi.item, key) {
				return slip.Character(ra[i])
			}
		} else {
			if fi.test.Call(s, slip.List{fi.item, key}, d2) != nil {
				return slip.Character(ra[i])
			}
		}
	}
	return nil
}
