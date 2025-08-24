// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
)

// PathSymbol is the symbol with a value of "path".
const PathSymbol = slip.Symbol("bag-path")

// Path is a JSONPath.
type Path jp.Expr

// String representation of the Object.
func (obj Path) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Path) Append(b []byte) []byte {
	b = append(b, "#<bag-path "...)
	b = jp.Expr(obj).Append(b)
	return append(b, '>')
}

// Simplify the Object into a path.Path.
func (obj Path) Simplify() interface{} {
	return jp.Expr(obj).String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj Path) Equal(other slip.Object) (eq bool) {
	if x, ok := other.(Path); ok {
		if len(obj) == len(x) {
			for i, v := range obj {
				if v != x[i] {
					return false
				}
			}
			return true
		}
	}
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Path) Hierarchy() []slip.Symbol {
	return []slip.Symbol{PathSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj Path) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// LoadForm returns a form that can be evaluated to create the object.
func (obj Path) LoadForm() slip.Object {
	return slip.List{slip.Symbol("make-bag-path"), slip.String(jp.Expr(obj).Append(nil))}
}
