// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"strconv"
)

// HashTableSymbol is the symbol with a value of "hash-table".
const HashTableSymbol = Symbol("hash-table")

func init() {
	DefConstant(HashTableSymbol, HashTableSymbol,
		`A _hash-table_ provides a mapping between a key and value. Keys can be a _string_, _symbol_, or _fixnum_.
Only the __eql__ _:test_ is supported.
`)
}

// HashTable of Objects.
type HashTable map[Object]Object

// String representation of the Object.
func (obj HashTable) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj HashTable) Append(b []byte) []byte {
	b = append(b, "#<hash-table eql "...)
	b = strconv.AppendInt(b, int64(len(obj)), 10)
	return append(b, "/-->"...)
}

// Simplify the Object into a []interface{}.
func (obj HashTable) Simplify() interface{} {
	out := map[string]interface{}{}
	for k, v := range obj {
		switch tk := k.(type) {
		case String:
			if v == nil {
				out[string(tk)] = nil
			} else {
				out[string(tk)] = v.Simplify()
			}
		case Symbol:
			if v == nil {
				out[string(tk)] = nil
			} else {
				out[string(tk)] = v.Simplify()
			}
		default:
			key := ObjectString(k)
			if v == nil {
				out[key] = nil
			} else {
				out[key] = v.Simplify()
			}
		}
	}
	return out
}

// Equal returns true if this Object and the other are equal in value.
func (obj HashTable) Equal(other Object) (eq bool) {
	if to, ok := other.(HashTable); ok {
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
func (obj HashTable) Hierarchy() []Symbol {
	return []Symbol{HashTableSymbol, TrueSymbol}
}

// Length returns the length of the object.
func (obj HashTable) Length() int {
	return len(obj)
}

// Eval returns self.
func (obj HashTable) Eval(s *Scope, depth int) Object {
	return obj
}
