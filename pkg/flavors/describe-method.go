// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"
	"io"
	"strings"

	"github.com/ohler55/slip"
)

var spaces = "                                                                                "

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
	if len(args) < 2 || 3 < len(args) {
		slip.PanicArgCount(f, 2, 3)
	}
	pos := len(args) - 1
	var cf *Flavor
	switch ta := args[pos].(type) {
	case slip.Symbol:
		cf = allFlavors[string(ta)]
	case *Flavor:
		cf = ta
	default:
		slip.PanicType("flavor argument to describe-method", ta, "symbol", "flavor")
	}
	if cf == nil {
		panic(fmt.Sprintf("%s is not a defined flavor.", args[pos]))
	}
	pos--
	meth, _ := args[pos].(slip.Symbol)
	ma := cf.methods[string(meth)]
	if len(ma) == 0 {
		panic(fmt.Sprintf("%s is not a method on %s.", args[pos], cf.name))
	}
	w := s.Get("*standard-output*").(io.Writer)
	pos--
	if pos == 0 {
		var ok bool
		if w, ok = args[0].(io.Writer); !ok {
			slip.PanicType("describe-method output-stream", args[0], "output-stream")
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
	b = append(b, cf.name...)
	b = append(b, off...)
	b = append(b, '\n')
	if len(spaces) < 2*len(ma) {
		spaces = strings.Repeat("  ", len(ma))
	}
	appendDaemon := func(m *method, caller slip.Caller, indent int, daemon string) {
		b = append(b, spaces[:indent]...)
		b = append(b, emp...)
		b = append(b, m.from.name...)
		b = append(b, off...)
		b = append(b, ' ')
		b = append(b, daemon...)
		b = append(b, '\n')
		if hd, ok := caller.(HasDocs); ok {
			b = slip.AppendDoc(b, hd.Docs(), 2+indent, right, ansi)
			b = append(b, '\n')
		}
	}
	for i, m := range ma {
		if m.wrap != nil {
			appendDaemon(m, m.wrap, (i+1)*2, ":whopper")
		}
	}
	for i, m := range ma {
		if m.before != nil {
			appendDaemon(m, m.before, (i+1)*2, ":before")
		}
	}
	for i, m := range ma {
		if m.primary != nil {
			appendDaemon(m, m.primary, (i+1)*2, ":primary")
			break
		}
	}
	for i := len(ma) - 1; 0 <= i; i-- {
		m := ma[i]
		if m.after != nil {
			appendDaemon(m, m.after, (i+1)*2, ":after")
		}
	}
	_, _ = w.Write(b)

	return slip.Novalue
}
