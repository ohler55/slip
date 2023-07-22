// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Search{Function: slip.Function{Name: "search", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "search",
			Args: []*slip.DocArg{
				{
					Name: "sequence-1",
					Type: "sequence",
					Text: "The sequence to match against the elements of _sequence-2_.",
				},
				{
					Name: "sequence-2",
					Type: "sequence",
					Text: "The sequence to search for _sequence-1_ in.",
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
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _sequence-1_ and _sequence-2_ to return a key for comparison.`,
				},
				{
					Name: "start1",
					Type: "fixnum",
					Text: `The index to the first element in _sequence-1_ to check. Defaults to 0`,
				},
				{
					Name: "end1",
					Type: "fixnum",
					Text: `The index to the last element in _sequence-1_ to check. Defaults to nil,
the length of the _sequence-1_.`,
				},
				{
					Name: "start2",
					Type: "fixnum",
					Text: `The index to the first element in _sequence-2_ to check. Defaults to 0`,
				},
				{
					Name: "end2",
					Type: "fixnum",
					Text: `The index to the last element in _sequence-2_ to check. Defaults to nil,
the length of the _sequence-2_.`,
				},
			},
			Return: "object",
			Text: `__search__ returns the index of the first match that satisfies _test_ or _nil_
if there is no match.`,
			Examples: []string{
				"(search '(2 3) '(1 2 3 4)) => 1",
			},
		}, &slip.CLPkg)
}

// Search represents the search function.
type Search struct {
	slip.Function
}

type searchVars struct {
	seq1    slip.Object
	start1  int
	end1    int
	start2  int
	end2    int
	test    slip.Caller
	key     slip.Caller
	fromEnd bool
}

// Call the function with the arguments provided.
func (f *Search) Call(s *slip.Scope, args slip.List, depth int) (index slip.Object) {
	var sv searchVars
	sv.setKeysItem(f, s, args, depth)
	switch ta := args[1].(type) {
	case nil:
		index = sv.inList(s, slip.List{}, depth)
	case slip.List:
		index = sv.inList(s, ta, depth)
	case slip.String:
		index = sv.inString(s, ta, depth)
	case slip.Vector:
		// TBD check sv.seq1 as vector
		index = sv.inList(s, slip.List(ta), depth)
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

func (sv *searchVars) setKeysItem(f slip.Object, s *slip.Scope, args slip.List, depth int) {
	slip.ArgCountCheck(f, args, 2, 16)
	sv.seq1 = args[0]
	sv.end1 = -1
	sv.end2 = -1
	pos := 2
	for ; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			sv.key = ResolveToCaller(s, args[pos+1], depth)
		case ":test":
			sv.test = ResolveToCaller(s, args[pos+1], depth)
		case ":start1":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				sv.start1 = int(num)
			} else {
				slip.PanicType("start1", args[pos+1], "fixnum")
			}
		case ":end1":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				sv.end1 = int(num)
			} else if args[pos+1] == nil {
				sv.end1 = -1
			} else {
				slip.PanicType("end1", args[pos+1], "fixnum")
			}
		case ":start2":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				sv.start2 = int(num)
			} else {
				slip.PanicType("start2", args[pos+1], "fixnum")
			}
		case ":end2":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				sv.end2 = int(num)
			} else if args[pos+1] == nil {
				sv.end2 = -1
			} else {
				slip.PanicType("end2", args[pos+1], "fixnum")
			}
		case ":from-end":
			sv.fromEnd = args[pos+1] != nil
		default:
			slip.PanicType("keyword", sym, ":key", ":test", ":start1", ":end1", ":start2", ":end2", ":from-end")
		}
	}
	if pos < len(args) {
		panic("extra arguments that are not keyword and value pairs")
	}
}

func (sv *searchVars) inList(s *slip.Scope, seq2 slip.List, depth int) slip.Object {
	var seq1 slip.List
	switch s1 := sv.seq1.(type) {
	case nil:
		seq1 = slip.List{}
	case slip.List:
		seq1 = s1
	case slip.Vector:
		seq1 = slip.List(s1)
	case slip.String:
		if 0 < len(s1) {
			return nil
		}
		return slip.Fixnum(0)
	default:
		slip.PanicType("sequence-1", s1, "sequence")
	}
	if sv.end1 < 0 {
		sv.end1 = len(seq1)
	} else if len(seq1) < sv.end1 {
		panic(fmt.Sprintf("bounding indices %d and %d are invalid for sequence of length %d",
			sv.start1, sv.end1, len(seq1)))
	}
	if sv.end2 < 0 {
		sv.end2 = len(seq2)
	} else if len(seq2) < sv.end2 {
		panic(fmt.Sprintf("bounding indices %d and %d are invalid for sequence of length %d",
			sv.start2, sv.end2, len(seq2)))
	}
	seq1 = seq1[sv.start1:sv.end1]
	seq2 = seq2[sv.start2:sv.end2]
	if len(seq1) == 0 {
		return slip.Fixnum(0)
	}
	if len(seq2) == 0 {
		return nil
	}
	d2 := depth + 1
	if sv.key != nil {
		kseq := make(slip.List, len(seq1))
		for i, v := range seq1 {
			kseq[i] = sv.key.Call(s, slip.List{v}, d2)
		}
		seq1 = kseq
		kseq = make(slip.List, len(seq2))
		for i, v := range seq2 {
			kseq[i] = sv.key.Call(s, slip.List{v}, d2)
		}
		seq2 = kseq
	}
	if sv.fromEnd {
		// TBD
	} else {
		first := seq1[0]
		for i, v2 := range seq2 {
			if sv.test == nil {
				if slip.ObjectEqual(first, v2) {
					if sv.listMatchNoTest(seq1, seq2[i:]) {
						return slip.Fixnum(sv.start2 + i)
					}
				}
			} else {
				if sv.test.Call(s, slip.List{first, v2}, d2) != nil {
					if sv.listMatchWithTest(s, seq1, seq2[i:], d2) {
						return slip.Fixnum(sv.start2 + i)
					}
				}
			}
		}
	}
	return nil
}

func (sv *searchVars) listMatchNoTest(seq1, seq2 slip.List) bool {
	if len(seq2) < len(seq1) {
		return false
	}
	for i, v1 := range seq1 {
		if !slip.ObjectEqual(v1, seq2[i]) {
			return false
		}
	}
	return true
}

func (sv *searchVars) listMatchWithTest(s *slip.Scope, seq1, seq2 slip.List, depth int) bool {
	if len(seq2) < len(seq1) {
		return false
	}
	for i, v1 := range seq1 {
		if sv.test.Call(s, slip.List{v1, seq2[i]}, depth) == nil {
			return false
		}
	}
	return true
}

func (sv *searchVars) inString(s *slip.Scope, seq2 slip.String, depth int) slip.Object {

	// TBD

	return nil
}
