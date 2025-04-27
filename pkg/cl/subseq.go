// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Subseq{Function: slip.Function{Name: "subseq", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "subseq",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: `A sequence to take the sub-sequence of.`,
				},
				{
					Name: "tree-start",
					Type: "fixnum",
					Text: `The start of the sub-sequence.`,
				},
				{Name: "&optional"},
				{
					Name: "end",
					Type: "fixnum",
					Text: `The end of the sub-sequence. (default is the end of the string or _nil_)`,
				},
			},
			Return: "sequence",
			Text:   `__subseq__ returns the sub-sequence of the _sequence_ from _start_ to _end_.`,
			Examples: []string{
				`(subseq '(a b c d) 1 3) => (b c)`,
			},
		}, &slip.CLPkg)
}

// Subseq represents the subseq function.
type Subseq struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Subseq) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	start, end, seq := f.getArgs(args)
	switch ta := seq.(type) {
	case slip.List:
		result = ta[start:end]
	case slip.String:
		ra := []rune(ta)
		result = slip.String(ra[start:end])
	case *slip.Vector:
		elements := ta.AsList()[start:end]
		result = slip.NewVector(len(elements), ta.ElementType(), nil, elements, ta.Adjustable())
	case slip.Octets:
		ba := []byte(ta)
		result = slip.Octets(ba[start:end])
	case *slip.BitVector:
		cnt := end - start
		bv := slip.BitVector{
			Bytes:     make([]byte, cnt/8+1),
			Len:       uint(cnt),
			FillPtr:   ta.FillPtr,
			CanAdjust: ta.CanAdjust,
		}
		for i := 0; i < cnt; i++ {
			bv.Put(uint(i), ta.At(uint(i+start)))
		}
		result = &bv
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *Subseq) Place(s *slip.Scope, args slip.List, value slip.Object) {
	start, end, seq := f.getArgs(args)
	switch ta := seq.(type) {
	case slip.List:
		if rep, ok := value.(slip.List); ok {
			cnt := end - start
			for i := 0; i < cnt && i < len(rep); i++ {
				ta[start+i] = rep[i]
			}
		} else {
			slip.PanicType("newvalue", value, "list")
		}
	case slip.String:
		slip.NewPanic("setf called on constant value %s", ta)
	case *slip.Vector:
		if rep, ok := value.(*slip.Vector); ok {
			cnt := end - start
			for i := 0; i < cnt && i < rep.Length(); i++ {
				ta.Set(rep.Get(i), start+i)
			}
		} else {
			slip.PanicType("newvalue", value, "list")
		}
	case slip.Octets:
		if rep, ok := value.(slip.Octets); ok {
			cnt := end - start
			for i := 0; i < cnt && i < len(rep); i++ {
				ta[start+i] = rep[i]
			}
		} else {
			slip.PanicType("newvalue", value, "octets")
		}
	case *slip.BitVector:
		if rep, ok := value.(*slip.BitVector); ok {
			cnt := end - start
			for i := 0; i < cnt && i < int(rep.Len); i++ {
				ta.Put(uint(i+start), rep.At(uint(i)))
			}
		} else {
			slip.PanicType("newvalue", value, "octets")
		}
	}
}

func (f *Subseq) getArgs(args slip.List) (start, end int, seq slip.Object) {
	slip.ArgCountCheck(f, args, 2, 3)
	if v, ok := args[1].(slip.Fixnum); ok && 0 <= v {
		start = int(v)
	} else {
		slip.PanicType("start", args[1], "fixnum")
	}
	end = -1
	if 2 < len(args) {
		switch ta := args[2].(type) {
		case nil:
			// leave as -1
		case slip.Fixnum:
			end = int(ta)
			if end < 0 {
				slip.PanicType("end", args[2], "non negative fixnum")
			}
		default:
			slip.PanicType("end", args[2], "non negative fixnum")
		}
	}
	switch ta := args[0].(type) {
	case slip.List:
		if end < 0 {
			end = len(ta)
		}
		if start < 0 || len(ta) < start || len(ta) < end {
			slip.NewPanic("indices %d and %d are out of bounds for list of length %d", start, end, len(ta))
		}
		seq = ta
	case slip.String:
		ra := []rune(ta)
		if end < 0 {
			end = len(ra)
		}
		if start < 0 || len(ra) < start || len(ra) < end {
			slip.NewPanic("indices %d and %d are out of bounds for string of length %d", start, end, len(ra))
		}
		seq = ta
	case *slip.Vector:
		size := ta.Length()
		if end < 0 {
			end = size
		}
		if start < 0 || size < start || size < end {
			slip.NewPanic("indices %d and %d are out of bounds for vector of length %d", start, end, size)
		}
		seq = ta
	case slip.Octets:
		ba := []byte(ta)
		if end < 0 {
			end = len(ba)
		}
		if start < 0 || len(ba) < start || len(ba) < end {
			slip.NewPanic("indices %d and %d are out of bounds for string of length %d", start, end, len(ba))
		}
		seq = ta
	case *slip.BitVector:
		if end < 0 {
			end = int(ta.Len)
		}
		if start < 0 || int(ta.Len) < start || int(ta.Len) < end {
			slip.NewPanic("indices %d and %d are out of bounds for string of length %d", start, end, ta.Len)
		}
		seq = ta
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}
