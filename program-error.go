// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ProgramErrorSymbol is the symbol with a value of "program-error".
const ProgramErrorSymbol = Symbol("program-error")

// NewProgramError creates a ProgramPanic (program-error) describing a program error.
func NewProgramError(format string, args ...any) Object {
	c := FindClass("program-error")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicProgram raises a ProgramPanic (program-error) describing a program
// error.
func PanicProgram(format string, args ...any) {
	panic(NewProgramError(format, args...))
}
