// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"
	"unicode"

	"github.com/ohler55/slip"
)

var stringCompareDocArgs = []*slip.DocArg{
	{
		Name: "string1",
		Type: "string|symbol|character",
		Text: "The first string to compare.",
	},
	{
		Name: "string2",
		Type: "string|symbol|character",
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
	switch ta := args[0].(type) {
	case slip.String:
		str1 = string(ta)
	case slip.Symbol:
		str1 = string(ta)
	case slip.Character:
		str1 = string([]rune{rune(ta)})
	default:
		slip.PanicType("string1", args[0], "string", "symbol", "character")
	}
	switch ta := args[1].(type) {
	case slip.String:
		str2 = string(ta)
	case slip.Symbol:
		str2 = string(ta)
	case slip.Character:
		str2 = string([]rune{rune(ta)})
	default:
		slip.PanicType("string2", args[1], "string", "symbol", "character")
	}
	end1 := len(str1)
	end2 := len(str2)
	for pos := 2; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			slip.NewPanic("%s missing an argument", sym)
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
		slip.NewPanic("start1 and end1 of %d, %d are not valid for a string of length %d", start1, end1, len(str1))
	}
	if end2 < start2 || len(str2) < end2 || start2 < 0 {
		slip.NewPanic("start2 and end2 of %d, %d are not valid for a string of length %d", start2, end2, len(str2))
	}
	if 0 < start1 || end1 < len(str1) {
		str1 = str1[start1:end1]
	}
	if 0 < start2 || end2 < len(str2) {
		str2 = str2[start2:end2]
	}
	return f.compare(str1, str2)
}

func compareStringFold(s1, s2 string) (i, diff int) {
	ra1 := []rune(s1)
	if s1 == s2 {
		return len(ra1), 0
	}
	ra2 := []rune(s2)
	var (
		r1 rune
		r2 rune
	)
	for i, r1 = range ra1 {
		if len(ra2) <= i {
			return i, 1 // s1 > s2
		}
		r2 = ra2[i]
		if r1 == r2 {
			continue
		}
		r1 = unicode.ToLower(r1)
		r2 = unicode.ToLower(r2)
		if r1 == r2 {
			continue
		}
		if r1 < r2 {
			return i, -1
		}
		return i, 1
	}
	i++
	if len(ra1) < len(ra2) {
		return i, -1
	}
	return
}
