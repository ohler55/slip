// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// TypeErrorSymbol is the symbol with a value of "type-error".
const TypeErrorSymbol = Symbol("type-error")

func init() {
	RegisterCondition("type-error", makeTypeError)
}

// TypeError is the interface for all type-errors.
type TypeError interface {
	Error

	// IsTypeError need not do anything other than exist.
	IsTypeError()

	// Datum is the object provided.
	Datum() Object

	// ExpectedTypes are the expected types. This deviates from common LSIP
	// where this is only one expected-type.
	ExpectedTypes() Object
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

// Hierarchy returns the class hierarchy as symbols for the instance.
func (tp *TypePanic) Hierarchy() []Symbol {
	return []Symbol{TypeErrorSymbol, ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}
}

// Eval the object.
func (tp *TypePanic) Eval(s *Scope, depth int) Object {
	return tp
}

// PanicType raises a TypePanic (type-error) describing an incorrect type
// being used.
func PanicType(use string, value Object, wants ...string) {
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
	if value != nil {
		b = append(b, ", a "...)
		b = append(b, value.Hierarchy()[0]...)
	}
	b = append(b, '.')
	expected := make(List, len(wants))
	for i, w := range wants {
		expected[i] = Symbol(w)
	}
	panic(&TypePanic{
		Panic:         Panic{Message: string(b)},
		datum:         value,
		expectedTypes: expected,
	})
}

func makeTypeError(args List) Condition {
	c := &TypePanic{}
	for k, v := range parseInitList(args) {
		switch k {
		case ":datum":
			c.datum = v
		case ":expected-type":
			if list, ok := v.(List); ok {
				c.expectedTypes = list
			} else {
				c.expectedTypes = List{v}
			}
		case ":context":
			c.context = v
		}
	}
	return c
}
