// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"github.com/ohler55/ojg"
	"github.com/ohler55/ojg/alt"
	"github.com/ohler55/ojg/sen"
)

// SimpleSymbol is the symbol with a value of "simple".
const SimpleSymbol = Symbol("simple")

// Simple is a Simple Object.
type Simple struct {
	Data interface{}
}

// String representation of the Object.
func (obj *Simple) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Simple) Append(b []byte) []byte {
	return append(b, sen.Bytes(obj.Data, &ojg.Options{Sort: true})...)
}

// Simplify the Object into a simple.Simple.
func (obj *Simple) Simplify() interface{} {
	return obj.Data
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Simple) Equal(other Object) bool {
	if to, ok := other.(*Simple); ok {
		return len(alt.Compare(obj.Data, to.Data)) == 0
	}
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Simple) Hierarchy() []Symbol {
	return []Symbol{SimpleSymbol, TrueSymbol}
}

// Eval returns self.
func (obj *Simple) Eval(s *Scope, depth int) Object {
	return obj
}
