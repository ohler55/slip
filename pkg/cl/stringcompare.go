// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

var stringCompareDocArgs = []*slip.DocArg{
	{
		Name: "string1",
		Type: "string",
		Text: "The first string to compare.",
	},
	{
		Name: "string2",
		Type: "string",
		Text: "The second string to compare.",
	},
	{Name: "&key"},
	{
		Name: "start1",
		Type: "fixnum",
		Text: "The index of the start of the portion of string1 to compare.",
	},
	{
		Name: "end1",
		Type: "fixnum",
		Text: "The index of the end of the portion of string1 to compare.",
	},
	{
		Name: "start2",
		Type: "fixnum",
		Text: "The index of the start of the portion of string2 to compare.",
	},
	{
		Name: "end2",
		Type: "fixnum",
		Text: "The index of the end of the portion of string2 to compare.",
	},
}

// stringCompare represents the string-compare function.
type stringCompare struct {
	slip.Function
	compare func(str1, str2 string) slip.Object
}

// Call the function with the arguments provided.
func (f *stringCompare) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 10)
	var (
		str1   string
		str2   string
		start1 int
		start2 int
	)
	if ss, ok := args[0].(slip.String); ok {
		str1 = string(ss)
	} else {
		slip.PanicType("string1", args[0], "string")
	}
	if ss, ok := args[1].(slip.String); ok {
		str2 = string(ss)
	} else {
		slip.PanicType("string2", args[1], "string")
	}
	end1 := len(str1)
	end2 := len(str2)
	for pos := 2; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			panic(fmt.Sprintf("%s missing an argument", sym))
		}
		var n slip.Fixnum
		switch strings.ToLower(string(sym)) {
		case ":start1":
			if n, ok = args[pos+1].(slip.Fixnum); ok {
				start1 = int(n)
			} else {
				slip.PanicType(string(sym), args[pos+1], "fixnum")
			}
		case ":end1":
			if n, ok = args[pos+1].(slip.Fixnum); ok {
				end1 = int(n)
			} else {
				slip.PanicType(string(sym), args[pos+1], "fixnum")
			}
		case ":start2":
			if n, ok = args[pos+1].(slip.Fixnum); ok {
				start2 = int(n)
			} else {
				slip.PanicType(string(sym), args[pos+1], "fixnum")
			}
		case ":end2":
			if n, ok = args[pos+1].(slip.Fixnum); ok {
				end2 = int(n)
			} else {
				slip.PanicType(string(sym), args[pos+1], "fixnum")
			}
		default:
			slip.PanicType("keyword", sym, ":start1", ":end1", ":start2", ":end2")
		}
	}
	if end1 < start1 || len(str1) < end1 || start1 < 0 {
		panic(fmt.Sprintf("start1 and end1 of %d, %d are not valid for a string of length %d", start1, end1, len(str1)))
	}
	if end2 < start2 || len(str2) < end2 || start2 < 0 {
		panic(fmt.Sprintf("start2 and end2 of %d, %d are not valid for a string of length %d", start2, end2, len(str2)))
	}
	if 0 < start1 || end1 < len(str1) {
		str1 = str1[start1:end1]
	}
	if 0 < start2 || end2 < len(str2) {
		str2 = str2[start2:end2]
	}
	return f.compare(str1, str2)
}
