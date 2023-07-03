// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReadFromString{Function: slip.Function{Name: "read-from-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "read-from-string",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The string to read from.",
				},
				{Name: "&optional"},
				{
					Name: "eof-error-p",
					Type: "boolean",
					Text: `If true an EOF error is raised when a read attempt is made at
the end of the stream. The default is _t_.`,
				},
				{
					Name: "eof-value",
					Type: "object",
					Text: "The value to return on EOF if _eof-error-p_ is nil.",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "The position in the string to start reading from. Default is zero.",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: `The position in the string that marks the end of the substring to read from.
Default is _nil_ meaning the end of the string.`,
				},
				{
					Name: "preserve-whitespace",
					Type: "boolean",
					Text: "If non-nil the returned _position_ will be immediately after the value read.",
				},
			},
			Return: "object,position",
			Text:   `__read-from-string__ reads from _string_ returns the value read.`,
			Examples: []string{
				`(read-from-string "123 ") => 123,3`,
			},
		}, &slip.CLPkg)
}

// ReadFromString represents the read-from-string function.
type ReadFromString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ReadFromString) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 9)
	ss, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	var (
		eofp  bool
		eofv  slip.Object
		start int
		end   int = -1
		pw    bool
		buf   []byte
	)
	if 1 < len(args) {
		ra := []rune(ss)
		var (
			optPos int
			sym    slip.Symbol
		)
		for pos := 1; pos < len(args); pos++ {
			if sym, ok = args[pos].(slip.Symbol); ok && 1 < len(sym) && sym[0] == ':' { // keyword
				pos++
				switch sym {
				case slip.Symbol(":start"):
					if num, ok2 := args[pos].(slip.Fixnum); ok2 {
						start = int(num)
					} else {
						slip.PanicType(":start", args[pos], "fixnum")
					}
				case slip.Symbol(":end"):
					if num, ok2 := args[pos].(slip.Fixnum); ok2 {
						end = int(num)
					} else if args[pos] != nil {
						slip.PanicType(":end", args[pos], "fixnum")
					}
				case slip.Symbol(":preserve-whitespace"):
					pw = args[pos] != nil
				default:
					slip.PanicType("keyword", sym, ":start", ":end", ":preserve-whitespace")
				}
			} else {
				switch optPos {
				case 0:
					eofp = args[pos] != nil
					optPos++
				case 1:
					eofv = args[pos]
					optPos++
				default:
					slip.PanicType("keyword", args[pos], "keyword")
				}
			}
		}
		if end < 0 {
			end = len(ra)
		}
		if start < 0 || len(ra) <= start || end < 0 || len(ra) < end || end < start {
			panic(fmt.Sprintf("the bounding indices %d and %d are not valid for string of length %d",
				start, end, len(ra)))
		}
		ra = ra[start:end]
		buf = []byte(string(ra))
	} else {
		buf = []byte(ss)
	}
	code, pos := slip.ReadOne(buf)
	pos += start
	if 0 < len(code) {
		if !pw {
		space:
			for ; pos < len(buf); pos++ {
				switch buf[pos] {
				case ' ', '\n', '\t', '\r':
				default:
					break space
				}
			}
		}
		return slip.Values{code[0], slip.Fixnum(pos)}
	}
	if eofp {
		panic(fmt.Sprintf("end of file on string %q", buf))
	}
	return slip.Values{eofv, slip.Fixnum(pos)}
}
