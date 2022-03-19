// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"strings"
	"unicode"
)

// FunctionSymbol is the symbol with a value of "function".
const FunctionSymbol = Symbol("function")

var funcCreators = map[string]func(args List) Object{}

type Function struct {
	Name string
	Args List
}

func Define(name string, creator func(args List) Object, doc *FuncDoc) {
	name = strings.ToLower(name)
	if _, has := funcCreators[name]; has {
		switch printer.Case {
		case upcaseKey:
			name = strings.ToUpper(name)
		case capitalizeKey:
			rn := []rune(name)
			rn[0] = unicode.ToUpper(rn[0])
			name = string(rn)
		}
		Warning("redefining %s", name)
	}
	funcCreators[name] = creator
	funcDocs[name] = doc
}

// String representation of the Object.
func (obj *Function) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Function) Append(b []byte) []byte {

	// TBD

	return b
}

// Simplify the function.
func (obj *Function) Simplify() interface{} {
	simple := make([]interface{}, 0, len(obj.Args)+1)
	simple = append(simple, obj.Name)
	for _, arg := range obj.Args {
		simple = append(simple, Simplify(arg))
	}
	return simple
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Function) Equal(other Object) bool {
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Function) Hierarchy() []Symbol {
	return []Symbol{FunctionSymbol, TrueSymbol}
}
