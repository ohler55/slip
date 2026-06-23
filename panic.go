// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"path/filepath"
	"strconv"
	"unsafe"
)

const (
	stackSymbol   = Symbol("stack")
	messageSymbol = Symbol("message")
)

var errorHierarchy = []Symbol{ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}

var (
	// StackTraceProvenance represents the *stack-trace-provenance* global
	// variable that controls how stack traces are displayed or output as
	// strings. If true provenance information of filename, line number, and
	// column are displayed. If not provenance information is available then
	// the function display is used.
	StackTraceProvenance bool

	// StackTraceFunction represents the *stack-trace-function* global
	// variable that controls how stack traces are displayed or output as
	// strings. Displays the function that was called.
	StackTraceFunction bool = true

	// *stack-trace-full-filenames* StackTraceFullFilenames represents the
	// *stack-trace-full-filenames* global variable that controls how stack
	// traces are displayed or output as strings. If *stack-trace-provenance*
	// is true then the full filename is displayed instead of just the base.
	StackTraceFullFilenames bool
)

// Panic is used to gather a stack trace when panic occurs.
type Panic struct {
	Message   string
	Condition Instance
	stack     []Funky
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

	stack := p.stack
	if p.Condition != nil {
		if sv, has := p.Condition.SlotValue(stackSymbol); has {
			stack = []Funky{}
			list, _ := sv.(List)
			for _, v := range list {
				if fn, ok := v.(Funky); ok {
					stack = append(stack, fn)
				}
			}
		}
	}
	return AppendStack(b, stack)
}

// AppendStack appends a stack based on the settings of the stack trace
// globals.
func AppendStack(b []byte, stack []Funky) []byte {
	if !StackTraceProvenance {
		for _, fn := range stack {
			b = append(b, "##  "...)
			b = append(b, fn.String()...)
			b = append(b, '\n')
		}
		return b
	}
	for _, fn := range stack {
		b = append(b, "## "...)
		prov := fn.Provenance()
		if prov != nil {
			if StackTraceFullFilenames {
				b = append(b, prov.Filepath...)
			} else {
				b = append(b, filepath.Base(prov.Filepath)...)
			}
			b = fmt.Appendf(b, ":%d.%d", prov.FirstLine+1, prov.FirstColumn)
		}
		if prov == nil || StackTraceFunction {
			b = append(b, ' ', ' ')
			b = append(b, fn.String()...)
		}
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
	if p.Condition != nil {
		if msg, _ := p.Condition.SlotValue(Symbol("message")); msg != nil {
			str := msg.String()
			if ss, ok := msg.(String); ok {
				str = string(ss)
			}
			return str
		}
	}
	return p.String()
}

// AppendToStack appends a function to the stack.
func (p *Panic) AppendToStack(fn *Function) {
	p.stack = append(p.stack, fn)
	if p.Condition != nil {
		if sv, has := p.Condition.SlotValue(stackSymbol); has {
			stack, _ := sv.(List)
			p.Condition.SetSlotValue(stackSymbol, append(stack, fn))
		}
	}
}

// Stack returns the call stack for the error.
func (p *Panic) Stack() []Funky {
	return p.stack
}

// WrapError creates a Panic that wraps a Instance which is expected to be a
// clos condition.
func WrapError(s *Scope, obj Instance, fn *Function) *Panic {
	p := Panic{Condition: obj}
	var stack List
	if sv, has := obj.SlotValue(stackSymbol); has {
		stack, _ = sv.(List)
	}
	if 0 < len(fn.Name) {
		p.stack = []Funky{fn}
		obj.SetSlotValue(stackSymbol, append(stack, fn))
	} else if 0 < len(stack) {
		p.stack = append(p.stack, fn)
	}
	if msg, has := obj.SlotValue(messageSymbol); has {
		if str, ok2 := msg.(String); ok2 {
			p.Message = string(str)
		}
	}
	return &p
}
