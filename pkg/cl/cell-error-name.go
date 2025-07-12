// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const nameSymbol = slip.Symbol("name")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CellErrorName{Function: slip.Function{Name: "cell-error-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cell-error-name",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "cell-error",
					Text: "The cell-error or subclass of cell-error to get the name of.",
				},
			},
			Return: "object",
			Text: `__cell-error-name__ returns the value of the _name_ slot in the _condition_
which must be of type _cell-error_ or inherit from _cell-error_.`,
			Examples: []string{
				`(cell-error-name (make-condition 'cell-error :name 'test)) => test)`,
			},
		}, &slip.CLPkg)
}

// CellErrorName represents the cell-error-name function.
type CellErrorName struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CellErrorName) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch cond := args[0].(type) {
	case slip.CellError:
		result = cond.Name()
	case slip.Instance:
		var has bool
		if result, has = cond.SlotValue(nameSymbol); !has {
			slip.PanicUnboundSlot(args[0], nameSymbol, "")
		}
	default:
		slip.PanicUnboundSlot(args[0], nameSymbol, "")
	}
	return
}
