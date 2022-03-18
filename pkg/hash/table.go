// Copyright (c) 2022, Peter Ohler, All rights reserved.

package hash

import (
	"strconv"

	"github.com/ohler55/slip"
)

// TableSymbol is the symbol with a value of "hashTable".
const TableSymbol = slip.Symbol("hash-table")

func init() {
	slip.DefConstant(TableSymbol, TableSymbol,
		`A _hash-table_ provides a mapping between a key and value. Keys can be a _string_, _symbol_, or _fixnum_.`)
}

// Table of Objects.
type Table map[slip.Object]slip.Object

// String representation of the Object.
func (obj Table) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Table) Append(b []byte) []byte {
	b = append(b, "#<HASH-TABLE :COUNT "...)
	b = strconv.AppendInt(b, int64(len(obj)), 10)
	return append(b, '>')
}

// Simplify the Object into a []interface{}.
func (obj Table) Simplify() interface{} {
	out := map[string]interface{}{}
	for k, v := range obj {
		switch tk := k.(type) {
		case slip.String:
			if v == nil {
				out[string(tk)] = nil
			} else {
				out[string(tk)] = v.Simplify()
			}
		case slip.Symbol:
			if v == nil {
				out[string(tk)] = nil
			} else {
				out[string(tk)] = v.Simplify()
			}
		default:
			slip.PanicType("Hash-Table keys", k, "string", "symbol")
		}
	}
	return out
}

// Equal returns true if this Object and the other are equal in value.
func (obj Table) Equal(other slip.Object) (eq bool) {
	if to, ok := other.(Table); ok {
		if len(obj) == len(to) {
			eq = true
			for k, v := range obj {
				vo, has := to[k]
				if !has {
					eq = false
					break
				}
				if v == nil {
					if vo != nil {
						eq = false
						break
					}
				} else {
					if !v.Equal(vo) {
						eq = false
						break
					}
				}
			}
		}
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Table) Hierarchy() []slip.Symbol {
	return []slip.Symbol{TableSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj Table) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}
