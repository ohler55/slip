// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

type Panic struct {
	Message string
	Stack   []string // TBD change to function
}

// Bytes returns the original error and stack in a format for display or
// writing.
func (p *Panic) Bytes() []byte {
	var b []byte

	b = append(b, "* "...)
	b = append(b, p.Message...)
	for _, line := range p.Stack {
		b = append(b, "  "...)
		b = append(b, line...)
		b = append(b, '\n')
	}
	return b
}

// Error returns the panic message.
func (p *Panic) Error() string {
	return p.Message
}

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

	panic(&Panic{Message: string(b)})
}
