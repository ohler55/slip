// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import "fmt"

// whopLocSymbol is the symbol with a value of "whopLoc".
const whopLocSymbol = Symbol("whopper-location")

type WhopLoc struct {
	Combinations []*Combination
	Current      int
}

// String representation of the Object.
func (wl *WhopLoc) String() string {
	return string(wl.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (wl *WhopLoc) Append(b []byte) []byte {
	return fmt.Appendf(b, "#<whopper-location %d>", wl.Current)
}

// Simplify by returning the string representation of the flavor.
func (wl *WhopLoc) Simplify() interface{} {
	return wl.String()
}

// Equal returns true if this Object and the other are equal in value.
func (wl *WhopLoc) Equal(other Object) (eq bool) {
	return wl == other
}

// Hierarchy returns the class hierarchy as symbols for the whopLoc.
func (wl *WhopLoc) Hierarchy() []Symbol {
	return []Symbol{whopLocSymbol, TrueSymbol}
}

// Eval returns self.
func (wl *WhopLoc) Eval(s *Scope, depth int) Object {
	return wl
}
