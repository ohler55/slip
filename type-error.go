// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// TypeErrorSymbol is the symbol with a value of "type-error".
const TypeErrorSymbol = Symbol("type-error")

var typeErrorHierarchy = []Symbol{TypeErrorSymbol, ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}

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

// NewTypeError returns a new TypePanic (type-error) describing an incorrect
// type being used.
func NewTypeError(use string, value Object, wants ...string) *TypePanic {
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
	var tp TypePanic
	tp.datum = value
	tp.expectedTypes = expected
	tp.context = String(use)
	tp.hierarchy = typeErrorHierarchy
	tp.Message = string(b)

	return &tp
}

// PanicType raises a TypePanic (type-error) describing an incorrect type
// being used.
func PanicType(use string, value Object, wants ...string) {
	panic(NewTypeError(use, value, wants...))
}

func makeTypeError(args List) Condition {
	var (
		use     String
		context Object
		value   Object
		wants   []string
		msg     String
	)
	for k, v := range ParseInitList(args) {
		switch k {
		case ":datum":
			value = v
		case ":expected-type":
			switch tv := v.(type) {
			case List:
				for _, e := range tv {
					switch te := e.(type) {
					case String:
						wants = append(wants, string(te))
					case Symbol:
						wants = append(wants, string(te))
					}

				}
			case String:
				wants = []string{string(tv)}
			case Symbol:
				wants = []string{string(tv)}
			}
		case ":context":
			use, _ = v.(String)
			context = v
		case ":message":
			msg, _ = v.(String)
		}
	}
	tp := NewTypeError(string(use), value, wants...)
	tp.context = context
	if 0 < len(msg) {
		tp.Message = string(msg)
	}
	return tp
}
