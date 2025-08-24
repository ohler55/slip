// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "strings"

// SymbolSymbol is the symbol with a value of "symbol".
const SymbolSymbol = Symbol("symbol")

// Symbol is a symbol Object.
type Symbol string

// String representation of the Object.
func (obj Symbol) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Symbol) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into a string.
func (obj Symbol) Simplify() any {
	return string(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj Symbol) Equal(other Object) bool {
	if sym, ok := other.(Symbol); ok && strings.EqualFold(string(obj), string(sym)) {
		return true
	}
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Symbol) Hierarchy() []Symbol {
	return []Symbol{SymbolSymbol, TrueSymbol}
}

// Eval the symbol and return it's binding in the current scope.
func (obj Symbol) Eval(s *Scope, depth int) Object {
	if 0 < len(obj) && obj[0] == ':' {
		return obj
	}
	return s.Get(obj)
}

// LoadForm returns a form that can be evaluated to create the object.
func (obj Symbol) LoadForm() Object {
	return obj
}
