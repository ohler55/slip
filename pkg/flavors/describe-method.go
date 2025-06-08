// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DescribeMethod{Function: slip.Function{Name: "describe-method", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "describe-method",
			Args: []*slip.DocArg{
				{
					Name: "flavor",
					Type: "symbol",
					Text: "The name of the flavor the method associated with.",
				},
				{
					Name: "method",
					Type: "symbol",
					Text: "The name of the method to describe.",
				},
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Text: `__describe-method__ function writes a description of the _method_ to the provided _output-stream_.
If the _output-stream_ is not provided then the _*standard-output*_ is used.`,
			Examples: []string{
				"(describe-method 'vanilla :print-self) ;; prints the documentation for :print-self",
			},
		}, &Pkg)
}

// DescribeMethod represents the describeMethod function.
type DescribeMethod struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *DescribeMethod) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 3)
	var hm HasMethods
	switch ta := args[0].(type) {
	case slip.Symbol:
		if hm, _ = slip.FindClass(string(ta)).(HasMethods); hm == nil {
			slip.PanicClassNotFound(ta, "%s is not a defined class or flavor.", ta)
		}
	case HasMethods:
		hm = ta
	default:
		slip.PanicType("flavor argument to describe-method", ta, "symbol", "flavor")
	}
	sc := hm.(slip.Class)
	meth, _ := args[1].(slip.Symbol)
	ma := hm.GetMethod(string(meth))
	if len(ma) == 0 {
		slip.PanicUnboundSlot(args[0], meth, "%s is not a method on %s.", args[1], args[0])
	}
	w := s.Get("*standard-output*").(io.Writer)
	if 2 < len(args) {
		var ok bool
		if w, ok = args[2].(io.Writer); !ok {
			slip.PanicType("describe-method output-stream", args[2], "output-stream")
		}
	}
	ansi := s.Get("*print-ansi*") != nil
	right := int(s.Get("*print-right-margin*").(slip.Fixnum))
	var (
		b   []byte
		emp string
		off string
	)
	if ansi {
		emp = bold
		off = colorOff
	}
	b = append(b, emp...)
	b = append(b, meth...)
	b = append(b, off...)
	b = append(b, " is a method of "...)
	b = append(b, emp...)
	b = append(b, sc.Name()...)
	b = append(b, off...)
	b = append(b, '\n')
	appendDaemon := func(m *Method, caller slip.Caller, daemon string) {
		b = append(b, "  "...)
		b = append(b, emp...)
		b = append(b, m.From.Name()...)
		b = append(b, off...)
		b = append(b, ' ')
		b = append(b, daemon...)
		b = append(b, '\n')
		if hd, ok := caller.(HasDocs); ok && 0 < len(hd.Docs()) {
			b = slip.AppendDoc(b, hd.Docs(), 4, right, ansi)
			b = append(b, '\n')
		}
	}
	for _, m := range ma {
		if m.wrap != nil {
			appendDaemon(m, m.wrap, ":whopper")
		}
	}
	for _, m := range ma {
		if m.before != nil {
			appendDaemon(m, m.before, ":before")
		}
	}
	for _, m := range ma {
		if m.primary != nil {
			appendDaemon(m, m.primary, ":primary")
			break
		}
	}
	for i := len(ma) - 1; 0 <= i; i-- {
		m := ma[i]
		if m.after != nil {
			appendDaemon(m, m.after, ":after")
		}
	}
	_, _ = w.Write(b)

	return slip.Novalue
}
