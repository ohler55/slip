// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SubstituteIf{Function: slip.Function{Name: "substitute-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "substitute-if",
			Args: []*slip.DocArg{
				{
					Name: "new",
					Type: "object",
					Text: "The replacement value.",
				},
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: "The function to call to determine if a value should be replaced.",
				},
				{
					Name: "sequence",
					Type: "sequemce",
					Text: "The sequence to replace values in.",
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _alist_ to return a key for comparison. The same function is also applied to _item_.`,
				},
				{
					Name: "from-end",
					Type: "boolean",
					Text: `If true start from the end. Only applies when _count_ is non-nil.`,
				},
				{
					Name: "start",
					Type: "fixnum",
					Text: `The index of the start of replacements.`,
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: `The index of the end (exclusive) of replacements.`,
				},
				{
					Name: "count",
					Type: "fixnum",
					Text: `The maximum number of replacements.`,
				},
			},
			Return: "sequence",
			Text:   `__substitute-if__ returns a copy _sequence_ with _old_ substitute-ifd with _new_.`,
			Examples: []string{
				"(setq lst '(a b c)",
				"(substitute-if 2 (lambda (v) (equal v 'b)) lst) => (a 2 c)",
				"lst => (a b c)",
			},
		}, &slip.CLPkg)
}

// SubstituteIf represents the substitute-if function.
type SubstituteIf struct {
	slip.Function
}

type subIfRep struct {
	s     *slip.Scope
	rep   slip.Object
	pc    slip.Caller
	kc    slip.Caller
	start int
	end   int
	count int
	depth int
	rev   bool
}

// Call the function with the arguments provided.
func (f *SubstituteIf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	sr := parseSubstituteIfArgs(f, s, args, depth)
	switch seq := args[2].(type) {
	case nil:
		// nothing to replace
	case slip.List:
		dup := make(slip.List, len(seq))
		copy(dup, seq)
		result = sr.replace(dup)
	case slip.String:
		if _, ok := sr.rep.(slip.Character); !ok {
			slip.PanicType("new", sr.rep, "character")
		}
		ra := []rune(seq)
		dup := make(slip.List, len(ra))
		for i, r := range ra {
			dup[i] = slip.Character(r)
		}
		_ = sr.replace(dup)
		for i, v := range dup {
			ra[i] = rune(v.(slip.Character))
		}
		result = slip.String(ra)
	case *slip.Vector:
		elements := seq.AsList()
		dup := make(slip.List, len(elements))
		copy(dup, elements)
		_ = sr.replace(dup)
		result = slip.NewVector(len(dup), seq.ElementType(), nil, dup, seq.Adjustable())
	default:
		slip.PanicType("sequence", seq, "sequence")
	}
	return
}

func parseSubstituteIfArgs(f slip.Object, s *slip.Scope, args slip.List, depth int) *subIfRep {
	slip.ArgCountCheck(f, args, 3, 15)
	sr := subIfRep{
		s:     s,
		rep:   args[0],
		pc:    ResolveToCaller(s, args[1], depth),
		count: -1,
		end:   -1,
	}
	kargs := args[3:]
	if v, ok := slip.GetArgsKeyValue(kargs, slip.Symbol(":key")); ok {
		sr.kc = ResolveToCaller(s, v, depth)
	}
	if v, ok := slip.GetArgsKeyValue(kargs, slip.Symbol(":from-end")); ok {
		sr.rev = v != nil
	}
	if v, ok := slip.GetArgsKeyValue(kargs, slip.Symbol(":start")); ok {
		switch tv := v.(type) {
		case slip.Fixnum:
			sr.start = int(tv)
			if sr.start < 0 {
				sr.start = 0
			}
		case nil:
			// leave at 0
		default:
			slip.PanicType(":start", v, "fixnum")
		}
	}
	if v, ok := slip.GetArgsKeyValue(kargs, slip.Symbol(":end")); ok {
		switch tv := v.(type) {
		case slip.Fixnum:
			sr.end = int(tv)
		case nil:
			// leave as -1
		default:
			slip.PanicType(":end", v, "fixnum")
		}
	}
	if v, ok := slip.GetArgsKeyValue(kargs, slip.Symbol(":count")); ok {
		switch tv := v.(type) {
		case slip.Fixnum:
			sr.count = int(tv)
		case nil:
			// leave as -1 for now
		default:
			slip.PanicType(":count", v, "fixnum")
		}
	}
	return &sr
}

func (sr *subIfRep) replace(seq slip.List) slip.Object {
	if sr.end < 0 || len(seq) < sr.end {
		sr.end = len(seq)
	}
	if sr.count < 0 {
		sr.count = len(seq)
	}
	if sr.rev {
		for i := sr.end - 1; sr.start <= i; i-- {
			if sr.maybe(seq, i) {
				break
			}
		}
	} else {
		for i := sr.start; i < sr.end; i++ {
			if sr.maybe(seq, i) {
				break
			}
		}
	}
	return seq
}

func (sr *subIfRep) maybe(seq slip.List, i int) bool {
	v := seq[i]
	if sr.kc != nil {
		v = sr.kc.Call(sr.s, slip.List{v}, sr.depth)
	}
	if sr.pc.Call(sr.s, slip.List{v}, sr.depth) != nil {
		seq[i] = sr.rep
	}
	sr.count--
	return sr.count <= 0
}
