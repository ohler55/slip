// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UnboundVariableSymbol is the symbol with a value of "unbound-variable".
const UnboundVariableSymbol = Symbol("unbound-variable")

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

// Hierarchy returns the class hierarchy as symbols for the instance.
func (uv *UnboundVariablePanic) Hierarchy() []Symbol {
	return []Symbol{
		UnboundVariableSymbol,
		CellErrorSymbol,
		ErrorSymbol,
		SeriousConditionSymbol,
		ConditionSymbol,
		TrueSymbol,
	}
}

// Eval the object.
func (uv *UnboundVariablePanic) Eval(s *Scope, depth int) Object {
	return uv
}

// PanicUnboundVariable raises a UnboundVariablePanic (unbound-variable)
// describing a unbound-variable error.
func PanicUnboundVariable(name Object, format string, args ...any) {
	panic(&UnboundVariablePanic{
		CellPanic: CellPanic{
			Panic: Panic{Message: fmt.Sprintf(format, args...)},
			name:  name,
		},
	})
}

func makeUnboundVariable(args List) Condition {
	c := &UnboundVariablePanic{}
	for k, v := range parseInitList(args) {
		if k == ":name" {
			c.name = v
		}
	}
	return c
}
