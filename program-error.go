// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ProgramErrorSymbol is the symbol with a value of "program-error".
const ProgramErrorSymbol = Symbol("program-error")

// ProgramErrorNew creates a ProgramPanic (program-error) describing a program error.
func ProgramErrorNew(s *Scope, depth int, format string, args ...any) Object {
	c := FindClass("program-error")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// ProgramPanic raises a ProgramError (program-error) describing a program
// error.
func ProgramPanic(s *Scope, depth int, format string, args ...any) {
	panic(ProgramErrorNew(s, depth, format, args...))
}
