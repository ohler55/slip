// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ParseErrorSymbol is the symbol with a value of "parse-error".
const ParseErrorSymbol = Symbol("parse-error")

// NewParseError creates a ParsePanic (parse-error) describing a parse error.
func NewParseError(format string, args ...any) Object {
	c := FindClass("parse-error")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicParse raises a ParsePanic (parse-error) describing a parse
// error.
func PanicParse(format string, args ...any) {
	panic(NewParseError(format, args...))
}
