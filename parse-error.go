// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ParseErrorSymbol is the symbol with a value of "parse-error".
const ParseErrorSymbol = Symbol("parse-error")

// ParseErrorNew creates a ParsePanic (parse-error) describing a parse error.
func ParseErrorNew(s *Scope, depth int, format string, args ...any) Object {
	c := FindClass("parse-error")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// ParsePanic raises a ParseError (parse-error) describing a parse
// error.
func ParsePanic(s *Scope, depth int, format string, args ...any) {
	panic(ParseErrorNew(s, depth, format, args...))
}
