// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// CellErrorSymbol is the symbol with a value of "cell-error".
const CellErrorSymbol = Symbol("cell-error")

// NewCellError raises a CellPanic (cell-error) describing a cell
// error.
func NewCellError(name Object, format string, args ...any) Object {
	c := FindClass("cell-error")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":name"), name,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicCell raises a CellPanic (cell-error) describing a cell
// error.
func PanicCell(name Object, format string, args ...any) {
	panic(NewCellError(name, format, args...))
}
