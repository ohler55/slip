// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

// whopLocSymbol is the symbol with a value of "whopLoc".
const whopLocSymbol = slip.Symbol("whopLoc")

func init() {
	slip.DefConstant(whopLocSymbol, whopLocSymbol, `A whopper location. Private for continue-whopper.`)
}

type whopLoc struct {
	methods []*method
	current int
}

// String representation of the Object.
func (obj *whopLoc) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *whopLoc) Append(b []byte) []byte {
	return append(b, "#<whopper-location>"...)
}

// Simplify by returning the string representation of the flavor.
func (obj *whopLoc) Simplify() interface{} {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *whopLoc) Equal(other slip.Object) (eq bool) {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the whopLoc.
func (obj *whopLoc) Hierarchy() []slip.Symbol {
	return []slip.Symbol{whopLocSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj *whopLoc) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}
