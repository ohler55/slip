// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"strconv"
	"unsafe"
)

const (
	stackSymbol   = Symbol("stack")
	messageSymbol = Symbol("message")
)

var errorHierarchy = []Symbol{ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}

// Panic is used to gather a stack trace when panic occurs.
type Panic struct {
	Message   string
	Condition Instance
	stack     []string
	Value     Object // used when the panic function is called
	Fatal     bool   // used in repl to indicate an exit should be made
}

// String returns the panic message.
func (p *Panic) String() string {
	return string(p.Append(nil))
}

// Append the object to a byte slice.
func (p *Panic) Append(b []byte) []byte {
	typeName := "error"
	if p.Condition != nil {
		typeName = string(p.Condition.Hierarchy()[0])
	}
	b = append(b, "#<"...)
	b = append(b, typeName...)
	b = append(b, ' ')
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(p))), 16)
	return append(b, '>')
}

// AppendFull appends the message and stack of the error to a byte slice.
func (p *Panic) AppendFull(b []byte) []byte {
	b = append(b, "## "...)
	b = append(b, p.Message...)
	b = append(b, '\n')
	if p.Condition != nil {
		if sv, has := p.Condition.SlotValue(stackSymbol); has {
			if stack, ok := sv.(List); ok {
				for _, line := range stack {
					b = append(b, "##  "...)
					b = append(b, ObjectString(line)...)
					b = append(b, '\n')
				}
				return b
			}
		}
	}
	for _, line := range p.stack {
		b = append(b, "##  "...)
		b = append(b, line...)
		b = append(b, '\n')
	}
	return b
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (p *Panic) Hierarchy() []Symbol {
	h := errorHierarchy
	if p.Condition != nil {
		h = p.Condition.Hierarchy()
	}
	return h
}

// Equal returns true if this Object and the other are equal in value.
func (p *Panic) Equal(other Object) bool {
	return p == other
}

// Eval the object.
func (p *Panic) Eval(s *Scope, depth int) Object {
	return p
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (p *Panic) Simplify() any {
	return string(p.Append(nil))
}

// Error returns the panic message.
func (p *Panic) Error() string {
	if 0 < len(p.Message) {
		return p.Message
	}
	return p.String()
}

// AppendToStack appends a function name and argument to the stack.
func (p *Panic) AppendToStack(name string, args List) {
	line := append(List{Symbol(name)}, args...)
	p.stack = append(p.stack, ObjectString(line))
	if p.Condition != nil {
		if sv, has := p.Condition.SlotValue(stackSymbol); has {
			stack, _ := sv.(List)
			p.Condition.SetSlotValue(stackSymbol, append(stack, line))
		}
	}
}

// Stack returns the call stack for the error.
func (p *Panic) Stack() []string {
	return p.stack
}

// WrapError creates a Panic that wraps a Instance which is expected to be a
// clos condition.
func WrapError(s *Scope, obj Instance, name string, args List) *Panic {
	line := append(List{Symbol(name)}, args...)
	p := Panic{Condition: obj}
	var stack List
	if sv, has := obj.SlotValue(stackSymbol); has {
		stack, _ = sv.(List)
	}
	if 0 < len(name) {
		p.stack = []string{ObjectString(line)}
		obj.SetSlotValue(stackSymbol, append(stack, line))
	} else if 0 < len(stack) {
		p.stack = append(p.stack, ObjectString(line))
	}

	// TBD check report slot
	if msg, has := obj.SlotValue(messageSymbol); has {
		if str, ok2 := msg.(String); ok2 {
			p.Message = string(str)
		}
	}
	return &p
}

// NewPanic returns a Panic object that can then be used with a call to panic.
func NewPanic(format string, args ...any) {
	panic(ErrorNew(NewScope(), 0, format, args...))
}
