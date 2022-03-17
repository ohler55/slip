// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// SymbolSymbol is the symbol with a value of "symbol".
const SymbolSymbol = Symbol("symbol")

func init() {
	DefConstant(SymbolSymbol, SymbolSymbol, `A _symbol_ names an _object_.`)
}

// Symbol is a symbol Object.
type Symbol string

// String representation of the Object.
func (obj Symbol) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Symbol) Append(b []byte) []byte {
	return append(b, obj...)
}

// Simplify the Object into a string.
func (obj Symbol) Simplify() interface{} {
	return string(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj Symbol) Equal(other Object) bool {
	return obj == other
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
