// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// TypePanic represents a type-error.
type TypePanic struct {
	Panic
	datum         Object
	expectedTypes List
}

// IsTypeError need not do anything other than exist.
func (tp *TypePanic) IsTypeError() {
}

// Datum is the object provided.
func (tp *TypePanic) Datum() Object {
	return tp.datum
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
