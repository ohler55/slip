// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"

	"github.com/ohler55/ojg/alt"
)

// FuncInfo stores information about a function.
type FuncInfo struct {
	Name   string
	Create func(args List) Object
	Doc    *FuncDoc
	Pkg    *Package // package interned in
	Kind   Symbol
	Aux    any
	Export bool
}

// String representation of the Object.
func (obj *FuncInfo) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *FuncInfo) Append(b []byte) []byte {
	b = append(b, "#<"...)
	b = append(b, obj.Kind...)
	b = append(b, ' ')
	b = append(b, printer.caseName(obj.Name)...)
	return append(b, '>')
}

// Simplify the Object into an int64.
func (obj *FuncInfo) Simplify() any {
	return map[string]any{
		"name": obj.Name,
		"pkg":  obj.Pkg.Name,
		"doc":  alt.Decompose(obj.Doc),
	}
}

// Equal returns true if this Object and the other are equal in value.
func (obj *FuncInfo) Equal(other Object) (eq bool) {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *FuncInfo) Hierarchy() []Symbol {
	if len(obj.Kind) == 0 {
		obj.Kind = FunctionSymbol
	}
	return []Symbol{obj.Kind, TrueSymbol}
}

// Eval returns self.
func (obj *FuncInfo) Eval(s *Scope, depth int) Object {
	return obj
}

// Apply evaluates with the need to evaluate the args.
func (obj *FuncInfo) Apply(s *Scope, args List, depth int) (result Object) {
	return obj.Create(args).Eval(s, depth)
}

// Describe the instance in detail.
func (obj *FuncInfo) Describe(b []byte, indent, right int, ansi bool) []byte {
	return obj.Doc.Describe(b, indent, right, ansi)
}

// FuncDocs returns the documentation for the object.
func (obj *FuncInfo) FuncDocs() *FuncDoc {
	return obj.Doc
}

// DefList returns a list that when evaluated defines then function described
// by this FuncInfo.
func (obj *FuncInfo) DefList() (def List) {
	fmt.Printf("*** %s doc kind: %q %q\n", obj.Name, obj.Kind, obj.Doc.Kind)

	switch obj.Kind {
	case MacroSymbol:
		def = append(def, Symbol("defmacro"))
	case LambdaSymbol:
		// TBD same as defun?
		fmt.Printf("*** lambda\n")
	case FlosSymbol:
		meth, _ := obj.Aux.(string)
		return List{Symbol("flosfun"), Symbol(obj.Name), Symbol(meth)}
	case GenericFunctionSymbol:
		// TBD
	default:
		def = append(def, Symbol("defun"))
	}
	def = append(def, Symbol(obj.Name))
	args := make(List, len(obj.Doc.Args))
	for i, da := range obj.Doc.Args {
		if da.Default == nil {
			args[i] = Symbol(da.Name)
		} else {
			args[i] = List{Symbol(da.Name), da.Default}
		}
	}
	def = append(def, args)
	if 0 < len(obj.Doc.Text) {
		def = append(def, String(obj.Doc.Text))
	}
	fun := obj.Create(nil).(Funky)
	if lam, ok := fun.Caller().(*Lambda); ok {
		def = append(def, lam.Forms...)
	} else {
		def = append(def, Symbol("..."))
	}
	return
}
