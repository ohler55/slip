// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Gensym{Function: slip.Function{Name: "gensym", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "gensym",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "x",
					Type: "string|fixnum",
					Text: `Either a prefix for the new symbol or the alternative to the counter.`,
				},
			},
			Return: "symbol",
			Text: `__gensym__ returns a new symbol. If _x_ is a _string_ then it becomes the
prefix for the generated _symbol_. If _x_ is a positive integer it is used as the numeric
portion of the generated _symbol_.

The _*gensym-counter*_ is incremented after the _symbol_ is generated if it was used in
the generation of the new _symbol_.
`,
			Examples: []string{
				`(gensym) => G99`,
			},
		}, &slip.CLPkg)
}

// Gensym represents the gensym function.
type Gensym struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Gensym) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 1)
	prefix := "G"
	var suffix slip.Fixnum
	if 0 < len(args) {
		switch ta := args[0].(type) {
		case slip.String:
			prefix = string(ta)
		case slip.Fixnum:
			if ta.Int64() <= 0 {
				slip.PanicType("x", ta, "string", "positive integer")
			}
			suffix = ta
		default:
			slip.PanicType("x", ta, "string", "positive integer")
		}
	}
	if suffix == 0 {
		var ok bool
		gsym := slip.Symbol("*gensym-counter*")
		if suffix, ok = s.Get(gsym).(slip.Fixnum); ok && 0 < suffix {
			s.Set(gsym, suffix+1)
		} else {
			slip.PanicType("*gensym-counter*", s.Get(gsym), "string", "positive integer")
		}
	}
	return slip.Symbol(fmt.Sprintf("%s%s", prefix, suffix))
}
