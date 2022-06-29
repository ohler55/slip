// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "github.com/ohler55/ojg/alt"

// FuncInfo stores information about a function.
type FuncInfo struct {
	Name    string
	Create  func(args List) Object
	Doc     *FuncDoc
	Pkg     *Package // package interned in
	BuiltIn bool
}

// String representation of the Object.
func (obj *FuncInfo) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *FuncInfo) Append(b []byte) []byte {
	b = append(b, "#<"...)
	b = append(b, printer.caseName("function")...)
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
	return []Symbol{FunctionSymbol, TrueSymbol}
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
	b = append(b, indentSpaces[:indent]...)
	b = append(b, "Lambda-List: ("...)
	for i, da := range obj.Doc.Args {
		if 0 < i {
			b = append(b, ' ')
		}
		if da.Default == nil {
			b = append(b, da.Name...)
		} else {
			b = append(b, '(')
			b = append(b, da.Name...)
			b = append(b, ' ')
			b = Append(b, da.Default)
			b = append(b, ')')
		}
	}
	b = append(b, ")\n"...)

	if 0 < len(obj.Doc.Return) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Return: "...)
		b = append(b, obj.Doc.Return...)
		b = append(b, '\n')
	}
	if 0 < len(obj.Doc.Text) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Description:\n"...)
		b = AppendDoc(b, obj.Doc.Text, indent+2, right, ansi)
		b = append(b, '\n')
	}
	if 0 < len(obj.Doc.Args) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Arguments:\n"...)
	}
	for _, da := range obj.Doc.Args {
		if da.Name[0] == '&' {
			continue
		}
		b = append(b, indentSpaces[:indent+2]...)
		if ansi {
			b = append(b, underline...)
			b = append(b, da.Name...)
			b = append(b, colorOff...)
		} else {
			b = append(b, da.Name...)
		}
		b = append(b, ": ["...)
		b = append(b, da.Type...)
		b = append(b, ']')
		if da.Default != nil {
			b = append(b, " = "...)
			b = da.Default.Append(b)
		}
		if 0 < len(da.Text) {
			b = append(b, '\n')
			b = AppendDoc(b, da.Text, indent+4, right, ansi)
		}
		b = append(b, '\n')
	}
	if 0 < len(obj.Doc.Examples) {
		b = append(b, '\n')
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Examples:\n"...)
		for _, ex := range obj.Doc.Examples {
			b = append(b, indentSpaces[:indent+2]...)
			b = append(b, ex...)
			b = append(b, '\n')
		}
	}
	return append(b, '\n')
}
