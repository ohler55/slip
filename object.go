// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"time"
	"unsafe"
)

// Object is the interface for all LISP entities other than nil.
type Object interface {
	fmt.Stringer

	// Append the object to a byte slice.
	Append(b []byte) []byte

	// Simplify the Object into simple go types of nil, bool, int64, float64,
	// string, []any, map[string]any, or time.Time.
	Simplify() any

	// Equal returns true if this Object and the other are equal in value.
	Equal(other Object) bool

	// Hierarchy returns the class hierarchy as symbols for the instance.
	Hierarchy() []Symbol

	// Eval the object.
	Eval(s *Scope, depth int) Object

	// LoadForm should return a form that can be evaluated to create the object
	// or panics if that is not possible.
	// LoadForm() Object
	// TBD keep trying
}

// ObjectString returns the string for an Object or "nil" if nil.
func ObjectString(obj Object) string {
	if IsNil(obj) {
		return "nil"
	}
	return obj.String()
}

// ObjectAppend appends Object or "nil" if nil.
func ObjectAppend(b []byte, obj Object) []byte {
	if obj == nil {
		return append(b, "nil"...)
	}
	return obj.Append(b)
}

// ObjectEqual compares two Object for equality returning true if they are
// equal.
func ObjectEqual(x, y Object) (eq bool) {
	if x == nil {
		return y == nil
	}
	return x.Equal(y)
}

// IsNil checks for a nil value of an interface. Go values have two components
// not exposed, a type component and a value component. Further reading:
// https://research.swtch.com/interfaces. To ascertain whether the value is
// nil we ignore the type component and just check if the value component is
// set to 0.
func IsNil(v any) bool {
	return (*[2]uintptr)(unsafe.Pointer(&v))[1] == 0
}

// SimpleObject creates an Object from simple data.
func SimpleObject(val any) (obj Object) {
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
		obj = Octet(tv)
	case uint16:
		obj = Fixnum(tv)
	case uint32:
		obj = Fixnum(tv)
	case uint64:
		obj = Fixnum(tv)

	case float32:
		obj = SingleFloat(tv)
	case float64:
		obj = DoubleFloat(tv)

	case string:
		obj = String(tv)
	case []byte:
		obj = String(tv)

	case time.Time:
		obj = Time(tv)

	case []any:
		list := make(List, 0, len(tv))
		for _, v := range tv {
			list = append(list, SimpleObject(v))
		}
		obj = list
	case map[string]any:
		list := make(List, 0, len(tv))
		for k, v2 := range tv {
			list = append(list, List{String(k), Tail{Value: SimpleObject(v2)}})
		}
		obj = list

	case *Panic:
		obj = tv.Value
		if obj == nil {
			obj = String(tv.Message)
		}
	case Object:
		obj = tv

	case error:
		obj = String(tv.Error())
	}
	return
}

// Simplify an Object.
func Simplify(obj Object) any {
	if obj == nil {
		return nil
	}
	return obj.Simplify()
}
