// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

const (
	// AmpBody is &body.
	AmpBody = "&body"
	// AmpKey is &key.
	AmpKey = "&key"
	// AmpOptional is &optional.
	AmpOptional = "&optional"
	// AmpRest is &rest.
	AmpRest = "&rest"
	// AmpAux is &aux.
	AmpAux = "&aux"
	// AmpAllowOtherKeys is &allow-other-keys
	AmpAllowOtherKeys = "&allow-other-keys"
)

// DocArg describes a function argument.
type DocArg struct {
	Name    string
	Type    string
	Text    string
	Default Object
}

// FuncDoc describes a function.
type FuncDoc struct {
	Name     string
	Args     []*DocArg
	Return   string // return type
	Text     string
	Examples []string
	Kind     Symbol
	NoExport bool
}

// HasFuncDocs is an interface for objects that have FuncDoc documentation.
type HasFuncDocs interface {
	// FuncDocs returns the documentation for the object.
	FuncDocs() *FuncDoc
}

func (fd *FuncDoc) getArg(name string) *DocArg {
	for _, a := range fd.Args {
		if a.Name == name {
			return a
		}
	}
	return nil
}

// LoadForm return a argument list for function or lambda args list.
func (fd *FuncDoc) LoadForm() Object {
	dl := make(List, len(fd.Args))
	for i, da := range fd.Args {
		switch {
		case fd.Kind == GenericFunctionSymbol || fd.Kind == MethodSymbol:
			switch {
			case 0 < len(da.Type):
				dl[i] = List{Symbol(da.Name), Symbol(da.Type)}
			case da.Default == nil:
				dl[i] = Symbol(da.Name)
			default:
				dl[i] = List{Symbol(da.Name), da.Default}
			}
		case da.Default == nil:
			dl[i] = Symbol(da.Name)
		default:
			dl[i] = List{Symbol(da.Name), da.Default}
		}
	}
	return dl
}

// Describe the instance in detail.
func (fd *FuncDoc) Describe(b []byte, indent, right int, ansi bool) []byte {
	b = append(b, indentSpaces[:indent]...)
	b = append(b, "Lambda-List: ("...)
	for i, da := range fd.Args {
		if 0 < i {
			b = append(b, ' ')
		}
		name := da.Name
		if 0 < len(name) && name[0] == ':' {
			name = name[1:]
		}
		if da.Default == nil {
			b = append(b, name...)
		} else {
			b = append(b, '(')
			b = append(b, name...)
			b = append(b, ' ')
			b = Append(b, da.Default)
			b = append(b, ')')
		}
	}
	b = append(b, ")\n"...)

	if 0 < len(fd.Return) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Return: "...)
		b = append(b, fd.Return...)
		b = append(b, '\n')
	}
	if 0 < len(fd.Text) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Documentation:\n"...)
		b = AppendDoc(b, fd.Text, indent+2, right, ansi)
		b = append(b, '\n')
	}
	if 0 < len(fd.Args) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Arguments:\n"...)
		for _, da := range fd.Args {
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
			if 0 < len(da.Type) {
				b = append(b, ": ["...)
				b = append(b, da.Type...)
				b = append(b, ']')
			}
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
	}
	if 0 < len(fd.Examples) {
		b = append(b, '\n')
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Examples:\n"...)
		for _, ex := range fd.Examples {
			b = append(b, indentSpaces[:indent+2]...)
			b = append(b, ex...)
			b = append(b, '\n')
		}
	}
	return append(b, '\n')
}
