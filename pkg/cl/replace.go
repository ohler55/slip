// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Replace{Function: slip.Function{Name: "replace", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "replace",
			Args: []*slip.DocArg{
				{
					Name: "sequence-1",
					Type: "sequence",
					Text: "A sequence to replace elements in.",
				},
				{
					Name: "sequence-2",
					Type: "sequence",
					Text: "A sequence to take replacement elements from.",
				},
				{Name: "&key"},
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
			},
			Return: "sequence",
			Text: `__replace__ elements in _sequence-1_ with elements from _sequence-2_.
The segment bounded _start1_ and _end1_ with the elements between _start2_ and _end2_ in
_sequence-2_. Unlike Common Lisp string are immutable so a copy of the string is returned instead.`,
			Examples: []string{
				"(replace '(1 2 3 4 5) '(5 4 3 2 1) :start1 2 :end1 4) => (1 2 5 4 5)",
			},
		}, &slip.CLPkg)
}

// Replace represents the replace function.
type Replace struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Replace) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 10)
	var (
		start1 int
		start2 int
		pos    int
	)
	end1 := -1
	end2 := -1
	for pos = 2; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":start1":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				start1 = int(num)
			} else {
				slip.PanicType("start1", args[pos+1], "fixnum")
			}
		case ":end1":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				end1 = int(num)
			} else if args[pos+1] == nil {
				end1 = -1
			} else {
				slip.PanicType("end1", args[pos+1], "fixnum")
			}
		case ":start2":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				start2 = int(num)
			} else {
				slip.PanicType("start2", args[pos+1], "fixnum")
			}
		case ":end2":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				end2 = int(num)
			} else if args[pos+1] == nil {
				end2 = -1
			} else {
				slip.PanicType("end2", args[pos+1], "fixnum")
			}
		default:
			slip.PanicType("keyword", sym, ":start1", ":start2", ":end1", ":end2")
		}
	}
	if pos < len(args) {
		slip.NewPanic("extra arguments that are not keyword and value pairs")
	}
	result = args[0]
	seq2 := seqToList(args[1], "sequence-2", start2, end2)
	switch seq1 := args[0].(type) {
	case nil:
	case slip.List:
		end1 = f.checkStartEnd(start1, end1, len(seq1))
		// TBD check seq1 == seq2
		for i, v := range seq2 {
			if end1 <= start1+i {
				break
			}
			seq1[start1+i] = v
		}
	case slip.String:
		ra := []rune(seq1)
		end1 = f.checkStartEnd(start1, end1, len(ra))
		for i, v := range seq2 {
			if end1 <= start1+i {
				break
			}
			if c, ok := v.(slip.Character); ok {
				ra[start1+i] = rune(c)
			} else {
				slip.PanicType("string sequence-1 element", v, "character")
			}
		}
		result = slip.String(ra)
	case *slip.Vector:
		end1 = f.checkStartEnd(start1, end1, seq1.Length())
		for i, v := range seq2 {
			if end1 <= start1+i {
				break
			}
			seq1.Set(v, start1+i)
		}
	case slip.Octets:
		end1 = f.checkStartEnd(start1, end1, len(seq1))
		for i, v := range seq2 {
			if end1 <= start1+i {
				break
			}
			seq1[start1+i] = byte(ToOctet(v).(slip.Octet))
		}
	case *slip.BitVector:
		end1 = f.checkStartEnd(start1, end1, int(seq1.Len))
		for i, v := range seq2 {
			if end1 <= start1+i {
				break
			}
			seq1.Set(v, start1+i)
		}
	default:
		slip.PanicType("sequence-1", seq1, "sequence")
	}
	return
}

func (f *Replace) checkStartEnd(start, end, size int) int {
	if size == 0 && start == 0 && end == -1 {
		return 0
	}
	if size <= start {
		slip.NewPanic("Start of %d is out of bounds for sequence-1 with length %d.", start, size)
	}
	if end == -1 {
		end = size
	} else {
		if size <= end {
			slip.NewPanic("End of %d is out of bounds for sequence-1 with length %d.", end, size)
		}
		if end < start {
			slip.NewPanic("End of %d is less than start of %d for sequence-1.", end, start)
		}
	}
	return end
}
