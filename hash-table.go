// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strconv"
)

// HashTableSymbol is the symbol with a value of "hashTable".
const HashTableSymbol = Symbol("hash-table")

// HashTable of Objects.
type HashTable map[Object]Object

// String representation of the Object.
func (obj HashTable) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj HashTable) Append(b []byte) []byte {
	b = append(b, "#<HASH-TABLE :COUNT "...)
	b = strconv.AppendInt(b, int64(len(obj)), 10)
	return append(b, '>')
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
			// TBD panic wrapper once stack is determined
			var ts string
			if k == nil {
				ts = "nil"
			} else {
				ts = string(k.Hierarchy()[0])
			}
			panic(fmt.Sprintf("Hash-Table keys must be strings or symbols to be Simplified. Encountered key %s a %s.",
				k, ts))
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
