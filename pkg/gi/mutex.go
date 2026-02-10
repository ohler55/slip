// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"strconv"
	"sync"
	"unsafe"

	"github.com/ohler55/slip"
)

// MutexSymbol is the symbol with a value of "mutex".
const MutexSymbol = slip.Symbol("mutex")

// Mutex is a chan of Objects.
type Mutex sync.Mutex

// String representation of the Object.
func (obj *Mutex) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Mutex) Append(b []byte) []byte {
	b = append(b, "#<mutex "...)
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(obj))), 16)
	return append(b, '>')
}

// Simplify by returning the string representation of the flavor.
func (obj *Mutex) Simplify() any {
	return string(obj.Append([]byte{}))
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Mutex) Equal(other slip.Object) (eq bool) {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the mutex.
func (obj *Mutex) Hierarchy() []slip.Symbol {
	return []slip.Symbol{MutexSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj *Mutex) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}
