// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WithStandardIoSyntax{
				Function: slip.Function{Name: "with-standard-io-syntax", Args: args, SkipEval: []bool{true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "with-standard-io-syntax",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__with-standard-io-syntax__ creates a local scope with all print and read variables set to
values that produce standard read and print behavior. Note that some of the variables from Common Lisp are
missing as they are not supported. The variables and values set are:
  *package*                    The CL-USER package
  *print-ansi*                 nil
  *print-array*                t
  *print-base*                 10
  *print-case*                 :downcase
  *print-circle*               nil
  *print-escape*               t
  *print-gensym*               t
  *print-length*               nil
  *print-level*                nil
  *print-lines*                nil
  *print-miser-width*          nil
  *print-pretty*               nil
  *print-radix*                nil
  *print-readably*             t
  *print-right-margin*         nil
  *read-base*                  10
  *read-default-float-format*  double-float
.`,
			Examples: []string{
				"(with-standard-io-syntax ()) => nil",
			},
		}, &slip.CLPkg)
}

// WithStandardIoSyntax represents the with-standard-io-syntax function.
type WithStandardIoSyntax struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WithStandardIoSyntax) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	ns := s.NewScope()
	for k, v := range map[string]slip.Object{
		"*package*":                   &slip.UserPkg,
		"*print-ansi*":                nil,
		"*print-array*":               slip.True,
		"*print-base*":                slip.Fixnum(10),
		"*print-case*":                slip.Symbol(":downcase"),
		"*print-circle*":              nil,
		"*print-escape*":              slip.True,
		"*print-gensym*":              slip.True,
		"*print-length*":              nil,
		"*print-level*":               nil,
		"*print-lines*":               nil,
		"*print-miser-width*":         slip.Fixnum(0),
		"*print-pretty*":              nil,
		"*print-radix*":               nil,
		"*print-readably*":            slip.True,
		"*print-right-margin*":        nil,
		"*read-base*":                 slip.Fixnum(10),
		"*read-default-float-format*": slip.Symbol("double-float"),
	} {
		ns.UnsafeLet(slip.Symbol(k), v)
	}
	d2 := depth + 1
	for i := 0; i < len(args); i++ {
		result = slip.EvalArg(ns, args, i, d2)
		switch result.(type) {
		case *slip.ReturnResult, *GoTo:
			return result
		}
	}
	return
}
