// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import "fmt"

// PrintNotReadbleSymbol is the symbol with a value of "print-not-readable".
const PrintNotReadbleSymbol = Symbol("print-not-readable")

// NewPrintNotReadble creates a FilePanic (file-error) describing a file error.
func NewPrintNotReadble(object Object, format string, args ...any) Object {
	c := FindClass("print-not-readable")
	obj := c.MakeInstance()
	obj.Init(NewScope(), List{
		Symbol(":object"), object,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicPrintNotReadble raises a print-not-readable.
func PanicPrintNotReadble(object Object, format string, args ...any) {
	panic(NewPrintNotReadble(object, format, args...))
}
