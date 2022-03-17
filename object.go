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

	// Simplify the Object into simple go types of nil, bool, int64, float64,
	// string, []interface{}, map[string]interface{}, or time.Time.
	Simplify() interface{}

	// Equal returns true if this Object and the other are equal in value.
	Equal(other Object) bool

	// Hierarchy returns the class hierarchy as symbols for the instance.
	Hierarchy() []Symbol

	// Eval the object.
	Eval(s *Scope, depth int) Object
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
	if x == nil {
		return y == nil
	}
	return x.Equal(y)
}

// SimpleObject creates an Object from simple data.
func SimpleObject(val interface{}) (obj Object) {
	switch tv := val.(type) {
	case bool:
		if tv {
			obj = True
		}
	case int:
		obj = Fixnum(tv)
	case int8:
		obj = Fixnum(tv)
	case int16:
		obj = Fixnum(tv)
	case int32:
		obj = Fixnum(tv)
	case int64:
		obj = Fixnum(tv)

	case uint:
		obj = Fixnum(tv)
	case uint8:
		obj = Fixnum(tv)
	case uint16:
		obj = Fixnum(tv)
	case uint32:
		obj = Fixnum(tv)
	case uint64:
		obj = Fixnum(tv)

	case float32:
		obj = Float(tv)
	case float64:
		obj = Float(tv)

	case string:
		obj = String(tv)
	case []byte:
		obj = String(tv)

	case time.Time:
		obj = Time(tv)

	case []interface{}:
		list := make(List, 0, len(tv))
		for i := len(tv) - 1; 0 <= i; i-- {
			list = append(list, SimpleObject(tv[i]))
		}
		obj = list
	case map[string]interface{}:
		list := make(List, 0, len(tv))
		for k, v2 := range tv {
			list = append(list, Cons{String(k), SimpleObject(v2)})
		}
		obj = list

	case Object:
		obj = tv

	case error:
		obj = String(tv.Error())
	}
	return
}
