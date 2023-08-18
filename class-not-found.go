// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ClassNotFoundSymbol is the symbol with a value of "unbound-slot".
const ClassNotFoundSymbol = Symbol("class-not-found")

var classNotFoundHierarchy = []Symbol{
	ClassNotFoundSymbol,
	CellErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("class-not-found", makeClassNotFound)
}

// ClassNotFound is the interface for the class-not-found error.
type ClassNotFound interface {
	CellError

	// IsClassNotFound need not do anything other than exist.
	IsClassNotFound()
}

// ClassNotFoundPanic represents a unbound-slot.
type ClassNotFoundPanic struct {
	CellPanic
	instance Object
}

// IsClassNotFound need not do anything other than exist.
func (uv *ClassNotFoundPanic) IsClassNotFound() {
}

// Eval the object.
func (uv *ClassNotFoundPanic) Eval(s *Scope, depth int) Object {
	return uv
}

// NewClassNotFound creates a new ClassNotFoundPanic (class-not-found)
// describing a class-not-found error.
func NewClassNotFound(name Object, format string, args ...any) *ClassNotFoundPanic {
	var cond ClassNotFoundPanic
	cond.hierarchy = classNotFoundHierarchy
	cond.name = name
	cond.Message = fmt.Sprintf(format, args...)
	return &cond
}

// PanicClassNotFound raises a ClassNotFoundPanic (unbound-slot)
// describing a unbound-slot error.
func PanicClassNotFound(name Object, format string, args ...any) {
	panic(NewClassNotFound(name, format, args...))
}

func makeClassNotFound(args List) Condition {
	var (
		name     Object
		instance Object
	)
	for k, v := range parseInitList(args) {
		switch k {
		case ":name":
			name = v
		case ":instance":
			instance = v
		}
	}
	c := NewClassNotFound(name, "class %s not found", name)
	c.instance = instance

	return c
}
