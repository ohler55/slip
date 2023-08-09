// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

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
