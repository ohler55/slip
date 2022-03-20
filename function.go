// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
	"unicode"
)

// FunctionSymbol is the symbol with a value of "function".
const FunctionSymbol = Symbol("function")

var funcCreators = map[string]func(args List) Object{}

// Function is the base type for most if not all functions.
type Function struct {
	Name string
	Args List
}

// Define a new golang function.
func Define(creator func(args List) Object, doc *FuncDoc) {
	name := strings.ToLower(doc.Name)
	if _, has := funcCreators[name]; has {
		Warning("redefining %s", caseName(name))
	}
	funcCreators[name] = creator
	funcDocs[name] = doc
}

// NewFunc creates a new instance of the named function with the arguments
// provided.
func NewFunc(name string, args List) Object {
	name = strings.ToLower(name)
	if create := funcCreators[name]; create != nil {
		return create(args)
	}
	panic(fmt.Sprintf("Function %s is not defined.", caseName(name)))
}

// String representation of the Object.
func (obj *Function) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Function) Append(b []byte) []byte {
	b = append(b, '(')
	b = append(b, obj.Name...)
	for _, arg := range obj.Args {
		b = append(b, ' ')
		b = Append(b, arg)
	}
	return append(b, ')')
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

// GetArgs returns the function arguments.
func (obj *Function) GetArgs() []Object {
	return obj.Args
}

// GetName returns the function name.
func (obj *Function) GetName() string {
	return obj.Name
}

func caseName(name string) string {
	switch printer.Case {
	case upcaseKey:
		name = strings.ToUpper(name)
	case capitalizeKey:
		rn := []rune(name)
		rn[0] = unicode.ToUpper(rn[0])
		name = string(rn)
	}
	return name
}
