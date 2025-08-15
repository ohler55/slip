// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

type unbound byte

// Unbound indicates a variable is unbound.
const Unbound = unbound(0)

// String representation of the Object.
func (obj unbound) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj unbound) Append(b []byte) []byte {
	b = append(b, "<unbound>"...)
	return fmt.Appendf(b, " 0x%02x", byte(obj))
}

// Simplify the Object into true.
func (obj unbound) Simplify() any {
	return nil
}

// Equal returns true if this Object and the other are equal in value.
func (obj unbound) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj unbound) Hierarchy() []Symbol {
	return []Symbol{Symbol("unbound")}
}

// Eval returns self.
func (obj unbound) Eval(s *Scope, depth int) Object {
	return obj
}
