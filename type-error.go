// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// TypeErrorSymbol is the symbol with a value of "type-error".
const TypeErrorSymbol = Symbol("type-error")

// TypeError is the interface for all type-errors.
type TypeError interface {
	Error

	// IsTypeError need not do anything other than exist.
	IsTypeError()

	// Datum is the object provided.
	Datum() Object

	// ExpectedTypes are the expected types. This deviates from common LSIP
	// where this is only one expected-type.
	ExpectedTypes() List

	// Context of the error.
	Context() Object
}

// TypePanic represents a type-error.
type TypePanic struct {
	Panic
	datum         Object
	context       Object
	expectedTypes List
}

// IsTypeError need not do anything other than exist.
func (tp *TypePanic) IsTypeError() {
}

// Datum is the object provided.
func (tp *TypePanic) Datum() Object {
	return tp.datum
}

// Context is the context provided.
func (tp *TypePanic) Context() Object {
	return tp.context
}

// ExpectedTypes are the expected types. This deviates from common LSIP
// where this is only one expected-type.
func (tp *TypePanic) ExpectedTypes() List {
	return tp.expectedTypes
}

// Equal returns true if this Object and the other are equal in value.
func (tp *TypePanic) Equal(other Object) bool {
	return tp == other
}

// Eval the object.
func (tp *TypePanic) Eval(s *Scope, depth int) Object {
	return tp
}

// NewTypeErrorObject returns a new type-error describing an incorrect type
// being used.
func NewTypeErrorObject(use string, value Object, wants ...string) Object {
	c := FindClass("type-error")
	obj := c.MakeInstance()
	xt := make(List, len(wants))
	for i, w := range wants {
		xt[i] = Symbol(w)
	}
	var b []byte

	b = append(b, use...)
	b = append(b, " must be a "...)
	for i, want := range wants {
		if 0 < i {
			if i == len(wants)-1 {
				b = append(b, " or "...)
			} else {
				b = append(b, ", "...)
			}
		}
		b = append(b, want...)
	}
	b = append(b, " not "...)
	b = append(b, ObjectString(value)...)
	if !IsNil(value) {
		b = append(b, ", a "...)
		b = append(b, value.Hierarchy()[0]...)
	}
	b = append(b, '.')

	obj.Init(NewScope(), List{
		Symbol(":datum"), value,
		Symbol(":expected-type"), xt,
		Symbol(":message"), String(b),
	}, 0)
	return obj
}

// PanicType raises a TypePanic (type-error) describing an incorrect type
// being used.
func PanicType(use string, value Object, wants ...string) {
	panic(NewTypeErrorObject(use, value, wants...))
}
