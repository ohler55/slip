// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeString{Function: slip.Function{Name: "make-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-string",
			Args: []*slip.DocArg{
				{
					Name: "size",
					Type: "fixnum",
					Text: `The number of characters for the new string.`,
				},
				{Name: "&key"},
				{
					Name: "initial-element",
					Type: "character",
					Text: `The character to build the new string from.`,
				},
				{
					Name: "element-type",
					Type: "symbol",
					Text: `The element type can be either _'character_ or _nil_.`,
				},
			},
			Return: "string",
			Text: `__make-string__ returns a new string of size _size_ and filled
with _initial-element_ if supplied.`,
			Examples: []string{
				`(make-string 5 :initial-element #\b) => "bbbbb"`,
			},
		}, &slip.CLPkg)
}

// MakeString represents the make-string function.
type MakeString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeString) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 5)
	size, ok := args[0].(slip.Fixnum)
	if !ok || size < 0 {
		slip.TypePanic(s, depth, "size", args[0], "fixnum")
	}
	var c slip.Character
	if 1 < len(args) {
		for pos := 1; pos < len(args); pos += 2 {
			sym, ok := args[pos].(slip.Symbol)
			if !ok {
				slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
			}
			if len(args)-1 <= pos {
				slip.NewPanic("%s missing an argument", sym)
			}
			switch strings.ToLower(string(sym)) {
			case ":initial-element":
				if c, ok = args[pos+1].(slip.Character); !ok {
					slip.TypePanic(s, depth, "initial-element", sym, "character")
				}
			case ":element-type":
				switch ta := args[pos+1].(type) {
				case nil:
					// ok
				case slip.Symbol:
					if !strings.EqualFold(string(ta), "character") {
						slip.TypePanic(s, depth, "element-type", args[pos+1], "'character", "nil")
					}
				default:
					slip.TypePanic(s, depth, "element-type", args[pos+1], "'character", "nil")
				}
			default:
				slip.TypePanic(s, depth, "keyword", sym, ":initial-element", ":element-type")
			}
		}
	}
	if c == slip.Character(0) || size == 0 {
		return slip.String("")
	}
	return slip.String(strings.Repeat(string([]rune{rune(c)}), int(size)))
}
