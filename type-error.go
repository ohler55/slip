// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// TypeErrorSymbol is the symbol with a value of "type-error".
const TypeErrorSymbol = Symbol("type-error")

// TypeErrorNew returns a new type-error describing an incorrect type being
// used.
func TypeErrorNew(s *Scope, depth int, use string, value Object, wants ...string) Object {
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

	obj.Init(s, List{
		Symbol(":datum"), value,
		Symbol(":expected-type"), xt,
		Symbol(":message"), String(b),
	}, depth)
	return obj
}

// TypePanic raises a TypeError (type-error) describing an incorrect type
// being used.
func TypePanic(s *Scope, depth int, use string, value Object, wants ...string) {
	panic(TypeErrorNew(s, depth, use, value, wants...))
}
