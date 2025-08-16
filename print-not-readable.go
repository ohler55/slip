// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import "fmt"

// PrintNotReadableSymbol is the symbol with a value of "print-not-readable".
const PrintNotReadableSymbol = Symbol("print-not-readable")

// NewPrintNotReadable creates a FilePanic (file-error) describing a file error.
func NewPrintNotReadable(object Object, format string, args ...any) Object {
	c := FindClass("print-not-readable")
	obj := c.MakeInstance()
	obj.Init(NewScope(), List{
		Symbol(":object"), object,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicPrintNotReadable raises a print-not-readable.
func PanicPrintNotReadable(object Object, format string, args ...any) {
	panic(NewPrintNotReadable(object, format, args...))
}
