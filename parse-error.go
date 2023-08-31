// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ParseErrorSymbol is the symbol with a value of "parse-error".
const ParseErrorSymbol = Symbol("parse-error")

var parseErrorHierarchy = []Symbol{ParseErrorSymbol, ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}

func init() {
	RegisterCondition("parse-error", makeParseError)
}

// ParseError is the interface for all parse-errors.
type ParseError interface {
	Error

	// IsParseError need not do anything other than exist.
	IsParseError()
}

// ParsePanic represents a parse-error.
type ParsePanic struct {
	Panic
}

// IsParseError need not do anything other than exist.
func (pp *ParsePanic) IsParseError() {
}

// Equal returns true if this Object and the other are equal in value.
func (pp *ParsePanic) Equal(other Object) bool {
	return pp == other
}

// Eval the object.
func (pp *ParsePanic) Eval(s *Scope, depth int) Object {
	return pp
}

// NewParseError creates a ParsePanic (parse-error) describing a parse error.
func NewParseError(format string, args ...any) *ParsePanic {
	var cond ParsePanic
	cond.hierarchy = parseErrorHierarchy
	cond.Message = fmt.Sprintf(format, args...)
	return &cond
}

// PanicParse raises a ParsePanic (parse-error) describing a parse
// error.
func PanicParse(format string, args ...any) {
	panic(NewParseError(format, args...))
}

func makeParseError(args List) Condition {
	var msg String
	for k, v := range ParseInitList(args) {
		if k == ":message" {
			msg, _ = v.(String)
		}
	}
	return NewParseError("%s", string(msg))
}
