// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ErrorSymbol is the symbol with a value of "error".
const ErrorSymbol = Symbol("error")

func init() {
	RegisterCondition("error", makeError)
}

// Error is the interface for all errors. It has no functions that provide
// useful information other than to indicate the type is an Error which is
// also an Object.
type Error interface {
	SeriousCondition
	error

	// IsError need not do anything other than exist.
	IsError()

	// AppendToStack appends a function name and argument to the stack.
	AppendToStack(name string, args List)
}

// Panic is used to gather a stack trace when panic occurs.
type Panic struct {
	SeriousCondition

	Message string
	Stack   []string
	Value   Object // used when the panic function is called
	Fatal   bool   // used in repl to indicate an exit should be made
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

func makeError(args List) Condition {
	msg := ""
	if 0 < len(args) {
		if ss, ok := args[0].(String); ok {
			msg = string(ss)
		}
	}
	return &Panic{Message: msg}
}
