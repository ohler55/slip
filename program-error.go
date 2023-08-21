// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ProgramErrorSymbol is the symbol with a value of "program-error".
const ProgramErrorSymbol = Symbol("program-error")

var programErrorHierarchy = []Symbol{
	ProgramErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("program-error", makeProgramError)
}

// ProgramError is the interface for all program-errors.
type ProgramError interface {
	Error

	// IsProgramError need not do anything other than exist.
	IsProgramError()
}

// ProgramPanic represents a program-error.
type ProgramPanic struct {
	Panic
}

// IsProgramError need not do anything other than exist.
func (pp *ProgramPanic) IsProgramError() {
}

// Equal returns true if this Object and the other are equal in value.
func (pp *ProgramPanic) Equal(other Object) bool {
	return pp == other
}

// Eval the object.
func (pp *ProgramPanic) Eval(s *Scope, depth int) Object {
	return pp
}

// NewProgramError creates a ProgramPanic (program-error) describing a program error.
func NewProgramError(format string, args ...any) *ProgramPanic {
	var cond ProgramPanic
	cond.hierarchy = programErrorHierarchy
	cond.Message = fmt.Sprintf(format, args...)
	return &cond
}

// PanicProgram raises a ProgramPanic (program-error) describing a program
// error.
func PanicProgram(format string, args ...any) {
	panic(NewProgramError(format, args...))
}

func makeProgramError(args List) Condition {
	var msg String

	for k, v := range parseInitList(args) {
		if k == ":message" {
			msg, _ = v.(String)
		}
	}
	return NewProgramError("%s", string(msg))
}
