// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UnboundVariablePanic represents a unboundVariable-error.
type UnboundVariablePanic struct {
	CellPanic
}

// IsUnboundVariableError need not do anything other than exist.
func (uv *UnboundVariablePanic) IsUnboundVariableError() {
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
func PanicUnboundVariable(name string, format string, args ...any) {
	panic(&UnboundVariablePanic{
		CellPanic: CellPanic{
			Panic: Panic{Message: fmt.Sprintf(format, args...)},
			name:  name,
		},
	})
}
