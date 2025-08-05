// Copyright (c) 2024, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Flosfun{Function: slip.Function{Name: "flosfun", Args: args, SkipEval: []bool{true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "flosfun",
			Args: []*slip.DocArg{
				{
					Name: "function-name",
					Type: "string|symbol",
					Text: "The name of the function to be defined.",
				},
				{
					Name: "method",
					Type: "keyword",
					Text: "The method to send to an instance.",
				},
				{Name: "&optional"},
				{
					Name: "doc-source",
					Type: "string|flavor",
					Text: "The source for documentation as either a string or a flavor to method documentation from.",
				},
			},
			Return: "symbol",
			Text: `__flosfun__ returns the name of the new function. A new function is create with a body that
sends the _method_ to the first argument which must be an instance of a flavor that has the specified method.`,
		}, &Pkg)
}

// Flosfun represents the makeInstance function.
type Flosfun struct {
	slip.Function
}

type flosWrap struct {
	slip.Function
	meth string
}

// Call the the function with the arguments provided.
func (f *Flosfun) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 3)
	var fname string
	switch ta := args[0].(type) {
	case slip.Symbol:
		fname = string(ta)
	case slip.String:
		fname = string(ta)
	case *cl.Quote:
		// TBD
		if sym, ok := ta.Args[0].(slip.Symbol); ok {
			fname = string(sym)
		}
	}
	if len(fname) == 0 {
		slip.PanicType("function-name", args[0], "symbol")
	}
	var (
		meth string
		docs any
	)
	if sym, ok := args[1].(slip.Symbol); ok && 1 < len(sym) && sym[0] == ':' {
		meth = strings.ToLower(string(sym))
	} else {
		slip.PanicType("method", args[1], "keyword")
	}
	if 2 < len(args) {
		switch ta := args[2].(type) {
		case slip.String:
			docs = string(ta)
		case *Flavor:
			m := ta.methods[meth]
			if m == nil {
				slip.PanicType("doc-source", args[2], "string", "flavor")
			}
			docs = m.Doc
		default:
			slip.PanicType("doc-source", args[2], "string", "flavor")
		}
	}
	FlosFun(fname, meth, docs, slip.CurrentPackage)

	return slip.Symbol(fname)
}

func (f *flosWrap) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	inst, ok := args[0].(*Instance)
	if !ok {
		slip.PanicType("instance", args[0], "instance")
	}
	return inst.Receive(s, f.meth, args[1:], depth)
}

// FlosFun creates a function that sends a method to an instance.
func FlosFun(fname, meth string, docs any, p *slip.Package) {
	fd := slip.FuncDoc{
		Name: fname,
		Args: []*slip.DocArg{
			{
				Name: "instance",
				Type: "Instance",
				Text: fmt.Sprintf("The instance to send the %s method to.", meth),
			},
		},
		Return: "Object",
		Kind:   slip.FlosSymbol,
	}
	switch td := docs.(type) {
	case string:
		fd.Text = fmt.Sprintf(`__%s__ sends %s to _instance_ %s`, fname, meth, td)
		fd.Args = append(fd.Args, &slip.DocArg{Name: "&rest"})
		fd.Args = append(fd.Args, &slip.DocArg{
			Name: "args",
			Type: "Object",
			Text: fmt.Sprintf("The argument to the %s method.", meth),
		})
	case *slip.FuncDoc:
		fd.Text = td.Text
		fd.Return = td.Return
		fd.Args = append(fd.Args, td.Args...)
	default:
		fd.Text = fmt.Sprintf(`__%s__ sends to _instance_`, fname)
		fd.Args = append(fd.Args, &slip.DocArg{Name: "&rest"})
		fd.Args = append(fd.Args, &slip.DocArg{
			Name: "args",
			Type: "Object",
			Text: fmt.Sprintf("The argument to the %s method.", meth),
		})
	}
	_ = p.Define(
		func(wargs slip.List) slip.Object {
			fw := flosWrap{Function: slip.Function{Name: fname, Args: wargs}, meth: meth}
			fw.Self = &fw
			return &fw
		},
		&fd,
		meth,
	)
	// slip.Define(
	// 	func(wargs slip.List) slip.Object {
	// 		fw := flosWrap{Function: slip.Function{Name: fname, Args: wargs}, meth: meth}
	// 		fw.Self = &fw
	// 		return &fw
	// 	}, &fd, p)
}
