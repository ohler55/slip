// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"time"
)

type Object interface {
	fmt.Stringer

	// Append the object to a byte slice.
	Append(b []byte) []byte
}

// ObjectString returns the string for an Object or "nil" if nil.
func ObjectString(obj Object) string {
	if obj == nil {
		return "nil"
	}
	return obj.String()
}

// ObjectEqual compares two Object for equality returning true if they are
// equal.
func ObjectEqual(x, y Object) (eq bool) {
	switch tx := x.(type) {
	case Integer:
		switch ty := y.(type) {
		case Integer:
			eq = tx == ty
		case Float:
			eq = Float(tx) == ty
		}
	case Float:
		switch ty := y.(type) {
		case Integer:
			eq = tx == Float(ty)
		case Float:
			eq = tx == ty
		}
	case Time:
		if ty, ok := y.(Time); ok {
			eq = time.Time(tx).Equal(time.Time(ty))
		}
	case List:
		if ty, ok := y.(List); ok && len(tx) == len(ty) {
			eq = true
			for i, xc := range tx {
				if !ObjectEqual(xc, ty[i]) {
					eq = false
					break
				}
			}
		}
	case Cons:
		if ty, ok := y.(Cons); ok && ObjectEqual(ty.Car(), tx.Car()) && ObjectEqual(ty.Cdr(), tx.Cdr()) {
			eq = true
		}
	default:
		eq = x == y
	}
	return
}

// SimpleObject creates an Object from simple data.
func SimpleToObject(val interface{}) (obj Object) {
	switch tv := val.(type) {
	case bool:
		if tv {
			obj = True
		}
	case int:
		obj = Integer(tv)
	case int8:
		obj = Integer(tv)
	case int16:
		obj = Integer(tv)
	case int32:
		obj = Integer(tv)
	case int64:
		obj = Integer(tv)

	case uint:
		obj = Integer(tv)
	case uint8:
		obj = Integer(tv)
	case uint16:
		obj = Integer(tv)
	case uint32:
		obj = Integer(tv)
	case uint64:
		obj = Integer(tv)

	case float32:
		obj = Float(tv)
	case float64:
		obj = Float(tv)

	case string:
		obj = String(tv)
	case []byte:
		obj = String(tv)

	case []interface{}:
		list := make(List, 0, len(tv))
		for _, vc := range tv {
			list = append(list, SimpleToObject(vc))
		}
		obj = list
	case map[string]interface{}:
		list := make(List, 0, len(tv))
		for k, v2 := range tv {
			list = append(list, Cons{String(k), SimpleToObject(v2)})
		}
		obj = list

	case Object:
		obj = tv

	case error:
		obj = String(tv.Error())
	}
	return
}
