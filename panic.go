// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "fmt"

// Panic is used to gather a stack trace when panic occurs.
type Panic struct {
	Message string
	Stack   []string
	Value   Object // used when the panic function is called
	Fatal   bool   // used in repl to indicate an exit should be made
}

// IsCondition indicates Panic is a Condition.
func (p *Panic) IsCondition() {
}

// IsSeriousCondition indicates Panic is a Condition.
func (p *Panic) IsSeriousCondition() {
}

// IsError indicates Panic is a Condition.
func (p *Panic) IsError() {
}

// Append the object to a byte slice.
func (p *Panic) Append(b []byte) []byte {
	b = append(b, "## "...)
	b = append(b, p.Message...)
	b = append(b, '\n')
	for _, line := range p.Stack {
		b = append(b, "##  "...)
		b = append(b, line...)
		b = append(b, '\n')
	}
	return b
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (p *Panic) Simplify() any {
	return p.Message
}

// Equal returns true if this Object and the other are equal in value.
func (p *Panic) Equal(other Object) bool {
	return p == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (p *Panic) Hierarchy() []Symbol {
	return []Symbol{ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}
}

// Eval the object.
func (p *Panic) Eval(s *Scope, depth int) Object {
	return p
}

// Error returns the panic message.
func (p *Panic) Error() string {
	return p.Message
}

// String returns the panic message.
func (p *Panic) String() string {
	return p.Message
}

// AppendToStack appends a function name and argument to the stack.
func (p *Panic) AppendToStack(name string, args List) {
	p.Stack = append(p.Stack, ObjectString(append(List{Symbol(name)}, args...)))
}

// NewPanic returns a Panic object that can then be used with a call to panic.
func NewPanic(format string, args ...any) {
	panic(&Panic{Message: fmt.Sprintf(format, args...)})
}

// ArgCountCheck panics if the number of arguments is outside the range
// specified.
func ArgCountCheck(obj Object, args List, min, max int) {
	if len(args) < min || (0 <= max && max < len(args)) {
		PanicArgCount(obj, min, max)
	}
}

// PanicArgCount raises a panic describing the wrong number of arguments to a
// function.
func PanicArgCount(obj Object, min, max int) {
	f := obj.(Funky)
	args := f.GetArgs()
	if min == max {
		if len(args) < min {
			panic(&Panic{
				Message: fmt.Sprintf("Too few arguments to %s. %d expected but got %d.",
					f.GetName(), min, len(args)),
			})
		} else {
			panic(&Panic{
				Message: fmt.Sprintf("Too many arguments to %s. %d expected but got %d.",
					f.GetName(), min, len(args)),
			})
		}
	}
	if len(args) < min {
		panic(&Panic{
			Message: fmt.Sprintf("Too few arguments to %s. At least %d expected but got %d.",
				f.GetName(), min, len(args)),
		})
	}
	panic(&Panic{
		Message: fmt.Sprintf("Too many arguments to %s. At most %d expected but got %d.",
			f.GetName(), max, len(args)),
	})
}
