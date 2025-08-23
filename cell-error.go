// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// CellErrorSymbol is the symbol with a value of "cell-error".
const CellErrorSymbol = Symbol("cell-error")

// CellErrorNew raises a CellError (cell-error) describing a cell error.
func CellErrorNew(s *Scope, depth int, name Object, format string, args ...any) Object {
	c := FindClass("cell-error")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":name"), name,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// CellPanic raises a CellError (cell-error) describing a cell error.
func CellPanic(s *Scope, depth int, name Object, format string, args ...any) {
	panic(CellErrorNew(s, depth, name, format, args...))
}
