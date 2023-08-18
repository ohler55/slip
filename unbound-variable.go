// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UnboundVariableSymbol is the symbol with a value of "unbound-variable".
const UnboundVariableSymbol = Symbol("unbound-variable")

var unboundVariableHierarchy = []Symbol{
	UnboundVariableSymbol,
	CellErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("unbound-variable", makeUnboundVariable)
}

// UnboundVariable is the interface for all unbound-variables.
type UnboundVariable interface {
	CellError

	// IsUnboundVariable need not do anything other than exist.
	IsUnboundVariable()
}

// UnboundVariablePanic represents a unboundVariable-error.
type UnboundVariablePanic struct {
	CellPanic
}

// IsUnboundVariableError need not do anything other than exist.
func (uv *UnboundVariablePanic) IsUnboundVariable() {
}

// Equal returns true if this Object and the other are equal in value.
func (uv *UnboundVariablePanic) Equal(other Object) bool {
	return uv == other
}

// Eval the object.
func (uv *UnboundVariablePanic) Eval(s *Scope, depth int) Object {
	return uv
}

// NewUnboundVariable returns a new UnboundVariablePanic (unbound-variable)
// describing a unbound-variable error.
func NewUnboundVariable(name Object, format string, args ...any) *UnboundVariablePanic {
	var cond UnboundVariablePanic
	cond.hierarchy = unboundVariableHierarchy
	cond.Message = fmt.Sprintf(format, args...)
	cond.name = name
	return &cond
}

// PanicUnboundVariable raises a UnboundVariablePanic (unbound-variable)
// describing a unbound-variable error.
func PanicUnboundVariable(name Object, format string, args ...any) {
	panic(NewUnboundVariable(name, format, args...))
}

func makeUnboundVariable(args List) Condition {
	var (
		name Object
		msg  String
	)
	for k, v := range parseInitList(args) {
		switch k {
		case ":name":
			name = v
		case ":message":
			msg, _ = v.(String)
		}
	}
	return NewUnboundVariable(name, "%s", string(msg))
}
