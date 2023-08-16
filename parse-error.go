// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ParseErrorSymbol is the symbol with a value of "parse-error".
const ParseErrorSymbol = Symbol("parse-error")

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

// Hierarchy returns the class hierarchy as symbols for the instance.
func (pp *ParsePanic) Hierarchy() []Symbol {
	return []Symbol{ParseErrorSymbol, ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}
}

// Eval the object.
func (pp *ParsePanic) Eval(s *Scope, depth int) Object {
	return pp
}

// PanicParse raises a ParsePanic (parse-error) describing a parse
// error.
func PanicParse(format string, args ...any) {
	panic(&ParsePanic{Panic: Panic{Message: fmt.Sprintf(format, args...)}})
}

func makeParseError(args List) Condition {
	msg := ""
	if 0 < len(args) {
		if ss, ok := args[0].(String); ok {
			msg = string(ss)
		}
	}
	return &ParsePanic{Panic: Panic{Message: msg}}
}
