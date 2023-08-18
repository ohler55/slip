// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ErrorSymbol is the symbol with a value of "error".
const ErrorSymbol = Symbol("error")

var errorHierarchy = []Symbol{ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}

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

	// AppendFull appends the message and stack of the error to a byte slice.
	AppendFull(b []byte) []byte
}

// Panic is used to gather a stack trace when panic occurs.
type Panic struct {
	SeriousConditionObj

	Message string
	Stack   []string
	Value   Object // used when the panic function is called
	Fatal   bool   // used in repl to indicate an exit should be made
}

// IsError indicates Panic is a Condition.
func (p *Panic) IsError() {
}

// AppendFull appends the message and stack of the error to a byte slice.
func (p *Panic) AppendFull(b []byte) []byte {
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

// Equal returns true if this Object and the other are equal in value.
func (p *Panic) Equal(other Object) bool {
	return p == other
}

// Eval the object.
func (p *Panic) Eval(s *Scope, depth int) Object {
	return p
}

// Error returns the panic message.
func (p *Panic) Error() string {
	return p.Message
}

// AppendToStack appends a function name and argument to the stack.
func (p *Panic) AppendToStack(name string, args List) {
	p.Stack = append(p.Stack, ObjectString(append(List{Symbol(name)}, args...)))
}

// NewError returns a Panic object that can then be used with a call to panic.
func NewError(format string, args ...any) *Panic {
	var p Panic
	p.hierarchy = errorHierarchy
	p.Message = fmt.Sprintf(format, args...)
	return &p
}

// NewPanic returns a Panic object that can then be used with a call to panic.
func NewPanic(format string, args ...any) {
	panic(NewError(format, args...))
}

func makeError(args List) Condition {
	var msg String
	for k, v := range parseInitList(args) {
		if k == ":message" {
			msg, _ = v.(String)
		}
	}
	return NewError("%s", string(msg))
}
