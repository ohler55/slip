// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "github.com/ohler55/ojg/alt"

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
