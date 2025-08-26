// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Documentation{Function: slip.Function{Name: "documentation", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.FunctionSymbol,
			Name: "documentation",
			Args: []*slip.DocArg{
				{
					Name: "x",
					Type: "symbol|function|flavor|class|list",
					Text: "An object.",
				},
				{
					Name: "doc-type",
					Type: "symbol",
					Text: "The type to get the documentation for.",
				},
			},
			Return: "string|nil",
			Text: `__documentation__ returns the documentation _x_ or what _x_ refers to. The supported
_doc-type_ values supported are _function_, _method_, _class_, _variable_, _type_, and _constant_.
`,
			Examples: []string{
				"(documentation '*standard-output* 'variable)",
				` => "is a stream used as the default output destination for writing."`,
			},
		}, &slip.CLPkg)
}

// Documentation represents the documentation function.
type Documentation struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Documentation) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)

	var docType slip.Object
	if dt, ok := args[1].(slip.Symbol); ok {
		docType = dt
	} else if args[1] != slip.True {
		slip.TypePanic(s, depth, "doc-type", args[1], "symbol", "t")
	}
	a := args[0]
Top:
	switch ta := a.(type) {
	case slip.Symbol:
		switch docType {
		case nil: // t
			f.unsup(docType, ta)
		case slip.Symbol("function"):
			if fi := slip.CurrentPackage.GetFunc(string(ta)); fi != nil {
				a = fi
				goto Top
			}
		case slip.Symbol("variable"):
			if vv := slip.CurrentPackage.GetVarVal(string(ta)); vv != nil && !vv.Const {
				result = slip.String(vv.Doc)
			}
		case slip.Symbol("constant"):
			if vv := slip.CurrentPackage.GetVarVal(string(ta)); vv != nil && vv.Const {
				result = slip.String(vv.Doc)
			}
		case slip.Symbol("type"):
			if c := slip.FindClass(string(ta)); c != nil {
				a = c
				goto Top
			}
		default:
			f.unsup(docType, ta)
		}
	case *slip.FuncInfo:
		if docType == nil || docType == slip.Symbol("function") {
			result = slip.String(ta.Doc.Text)
		}
	case *slip.Package:
		if docType == nil {
			result = slip.String(ta.Doc)
		}
	case slip.Class:
		if docType == nil || docType == slip.Symbol("type") || docType == slip.Symbol("class") {
			result = slip.String(ta.Documentation())
		}
	case *slip.Method:
		if ta.Doc != nil && (docType == nil || docType == slip.Symbol("method")) {
			result = slip.String(ta.Doc.Text)
		}
	default:
		f.unsup(docType, ta)
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *Documentation) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 2, 2)
	doc, ok := value.(slip.String)
	if !ok {
		slip.TypePanic(s, 0, "new-value", value, "string")
	}
	var docType slip.Object
	if dt, ok := args[1].(slip.Symbol); ok {
		docType = dt
	} else if args[1] != slip.True {
		slip.TypePanic(s, 0, "doc-type", args[1], "symbol", "t")
	}
	a := args[0]
Top:
	switch ta := a.(type) {
	case slip.Symbol:
		switch docType {
		case nil: // t
			f.unsup(docType, ta)
		case slip.Symbol("function"):
			if fi := slip.CurrentPackage.GetFunc(string(ta)); fi != nil {
				a = fi
				goto Top
			}
		case slip.Symbol("variable"):
			if vv := slip.CurrentPackage.GetVarVal(string(ta)); vv != nil && !vv.Const {
				vv.Doc = string(doc)
			}
		case slip.Symbol("constant"):
			if vv := slip.CurrentPackage.GetVarVal(string(ta)); vv != nil && vv.Const {
				vv.Doc = string(doc)
			}
		case slip.Symbol("type"):
			if c := slip.FindClass(string(ta)); c != nil {
				a = c
				goto Top
			}
		default:
			f.unsup(docType, ta)
		}
	case *slip.FuncInfo:
		if docType == nil || docType == slip.Symbol("function") {
			ta.Doc.Text = string(doc)
		}
	case *slip.Package:
		if docType == nil {
			ta.Doc = string(doc)
		}
	case slip.Class:
		if docType == slip.Symbol("type") {
			ta.SetDocumentation(string(doc))
		}
	default:
		f.unsup(docType, ta)
	}
}

func (f *Documentation) unsup(docType, val slip.Object) {
	slip.ErrorPanic(slip.NewScope(), 0, "Unsupported documentation: doc-type %s for type %T", docType, val)
}
