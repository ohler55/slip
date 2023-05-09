// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

var stringModifyDocArgs = []*slip.DocArg{
	{
		Name: "string",
		Type: "string",
		Text: "The string to modify.",
	},
	{Name: "&key"},
	{
		Name: "start",
		Type: "fixnum",
		Text: "The index of the start of the portion of the string to modify.",
	},
	{
		Name: "end",
		Type: "fixnum",
		Text: "The index of the end of the portion of the string to modify.",
	},
}

// stringModify represents the string-modify function.
type stringModify struct {
	slip.Function
	modify func(str string) string
}

// Call the function with the arguments provided.
func (f *stringModify) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 5)
	var (
		str   string
		start int
	)
	if ss, ok := args[0].(slip.String); ok {
		str = string(ss)
	} else {
		slip.PanicType("string", args[0], "string")
	}
	end := len(str)
	for pos := 1; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			panic(fmt.Sprintf("%s missing an argument", sym))
		}
		var n slip.Fixnum
		switch strings.ToLower(string(sym)) {
		case ":start":
			if n, ok = args[pos+1].(slip.Fixnum); ok {
				start = int(n)
			} else {
				slip.PanicType(string(sym), args[pos+1], "fixnum")
			}
		case ":end":
			if n, ok = args[pos+1].(slip.Fixnum); ok {
				end = int(n)
			} else {
				slip.PanicType(string(sym), args[pos+1], "fixnum")
			}
		default:
			slip.PanicType("keyword", sym, ":start", ":end")
		}
	}
	if end < start || len(str) < end || start < 0 {
		panic(fmt.Sprintf("start and end of %d, %d are not valid for a string of length %d", start, end, len(str)))
	}
	if 0 < start || end < len(str) {
		ra := []rune(str)
		buf := make([]rune, 0, len(ra))
		buf = append(buf, ra[:start]...)
		buf = append(buf, []rune(f.modify(string(ra[start:end])))...)
		buf = append(buf, ra[end:]...)
		result = slip.String(buf)
	} else {
		result = slip.String(f.modify(str))
	}
	return
}
