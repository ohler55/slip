// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Mismatch{Function: slip.Function{Name: "mismatch", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "mismatch",
			Args: []*slip.DocArg{
				{
					Name: "sequence-1",
					Type: "sequence",
					Text: "A sequence to compare.",
				},
				{
					Name: "sequence-2",
					Type: "sequence",
					Text: "A sequence to compare.",
				},
				{Name: "&key"},
				{
					Name: "from-end",
					Type: "boolean",
					Text: `If true the comparison is in reverse or or from the end to the start.`,
				},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns a boolean to
indicate a match. The default is _equal_`,
				},
				{
					Name: "start1",
					Type: "fixnum",
					Text: `The index to the first element in sequence-1 to check. Defaults to 0`,
				},
				{
					Name: "start2",
					Type: "fixnum",
					Text: `The index to the first element in sequence-2 to check. Defaults to 0`,
				},
				{
					Name: "end1",
					Type: "fixnum",
					Text: `The index to the last element in sequence-1 to check. Defaults to nil,
the length of _sequence-1_.`,
				},
				{
					Name: "end2",
					Type: "fixnum",
					Text: `The index to the last element in sequence-2 to check. Defaults to nil,
the length of _sequence-2_.`,
				},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in each sequence to return a key for comparison and is used for both the sequence-1 and the sequence-2.`,
				},
			},
			Return: "fixnum|nil",
			Text: `__mismatch__ checks two sequences for a mismatch by comparing _sequence-1_ and
_sequence-2_ element-wise.


If _sequence-1_ and _sequence-2_ are of equal length and match in every
element, the result is false. Otherwise, the result is a non-negative integer,
the index within sequence-1 of the leftmost or rightmost position, depending
on from-end, at which the two subsequences fail to match. If one subsequence
is shorter than and a matching prefix of the other, the result is the index
relative to _sequence-1_ beyond the last position tested.


If _:from-end_ is true, then one plus the index of the rightmost position in
which the sequences differ is returned. In effect, the subsequences are
aligned at their right-hand ends; then, the last elements are compared, the
penultimate elements, and so on. The index returned is an index relative to
_sequence-1_.
`,
			Examples: []string{
				"(mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t) => 3",
			},
		}, &slip.CLPkg)
}

// Mismatch represents the mismatch function.
type Mismatch struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Mismatch) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 16)
	var (
		start1  int
		start2  int
		test    slip.Caller
		key     slip.Caller
		fromEnd bool
		pos     int
	)
	end1 := -1
	end2 := -1
	for pos = 2; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			key = ResolveToCaller(s, args[pos+1], depth)
		case ":test":
			test = ResolveToCaller(s, args[pos+1], depth)
		case ":start1":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				start1 = int(num)
			} else {
				slip.TypePanic(s, depth, "start1", args[pos+1], "fixnum")
			}
		case ":end1":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				end1 = int(num)
			} else if args[pos+1] == nil {
				end1 = -1
			} else {
				slip.TypePanic(s, depth, "end1", args[pos+1], "fixnum")
			}
		case ":start2":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				start2 = int(num)
			} else {
				slip.TypePanic(s, depth, "start2", args[pos+1], "fixnum")
			}
		case ":end2":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				end2 = int(num)
			} else if args[pos+1] == nil {
				end2 = -1
			} else {
				slip.TypePanic(s, depth, "end2", args[pos+1], "fixnum")
			}
		case ":from-end":
			fromEnd = args[pos+1] != nil
		default:
			slip.TypePanic(s, depth, "keyword", sym, ":key", ":test", ":start1", ":start2", ":end1", ":end2", ":from-end")
		}
	}
	if pos < len(args) {
		slip.ErrorPanic(s, depth, "extra arguments that are not keyword and value pairs")
	}
	seq1 := seqToList(s, args[0], "sequence-1", start1, end1, depth)
	seq2 := seqToList(s, args[1], "sequence-2", start2, end2, depth)
	d2 := depth + 1
	if fromEnd {
		for i := 1; i <= len(seq1); i++ {
			v1 := seq1[len(seq1)-i]
			if len(seq2) < i {
				return slip.Fixnum(len(seq1) - i + start1 + 1)
			}
			v2 := seq2[len(seq2)-i]
			if key != nil {
				v1 = key.Call(s, slip.List{v1}, d2)
				v2 = key.Call(s, slip.List{v2}, d2)
			}
			if test == nil {
				if !slip.ObjectEqual(v1, v2) {
					return slip.Fixnum(i + start1)
				}
			} else {
				if test.Call(s, slip.List{v1, v2}, d2) == nil {
					return slip.Fixnum(i + start1)
				}
			}
		}
		if len(seq1) < len(seq2) {
			return slip.Fixnum(start1)
		}
	} else {
		for i, v1 := range seq1 {
			if len(seq2) <= i {
				return slip.Fixnum(i + start1)
			}
			v2 := seq2[i]
			if key != nil {
				v1 = key.Call(s, slip.List{v1}, d2)
				v2 = key.Call(s, slip.List{v2}, d2)
			}
			if test == nil {
				if !slip.ObjectEqual(v1, v2) {
					return slip.Fixnum(i + start1)
				}
			} else {
				if test.Call(s, slip.List{v1, v2}, d2) == nil {
					return slip.Fixnum(i + start1)
				}
			}
		}
		if len(seq1) < len(seq2) {
			return slip.Fixnum(len(seq1) + start1)
		}
	}
	return nil
}

func seqToList(s *slip.Scope, seq slip.Object, name string, start, end, depth int) (list slip.List) {
	switch ts := seq.(type) {
	case nil:
	case slip.List:
		list = ts
	case slip.String:
		ra := []rune(ts)
		list = make(slip.List, len(ra))
		for i, r := range ra {
			list[i] = slip.Character(r)
		}
	case slip.VectorLike:
		list = ts.AsList()
	default:
		slip.TypePanic(s, depth, name, ts, "sequence")
	}
	// Special case.
	if start == 0 && end == -1 && len(list) == 0 {
		return
	}
	if len(list) <= start {
		slip.ErrorPanic(s, depth, "Start of %d is out of bounds for %s with length %d.", start, name, len(list))
	}
	if end == -1 {
		end = len(list)
	} else {
		if len(list) < end {
			slip.ErrorPanic(s, depth, "End of %d is out of bounds for %s with length %d.", end, name, len(list))
		}
		if end < start {
			slip.ErrorPanic(s, depth, "End of %d is less than start of %d for %s.", end, start, name)
		}
	}
	return list[start:end]
}
