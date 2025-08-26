// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"math"
	"math/big"
	"strings"

	"github.com/ohler55/slip"
)

// . 0123456789abcdef0123456789abcdef
const intBaseMap = "" +
	".........__.._.................." + // 0x00
	"_..........+.-..\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09......" + // 0x20
	".\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23....." + // 0x40
	".\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23....." //   0x60

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ParseInteger{Function: slip.Function{Name: "parse-integer", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parse-integer",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The string to parse.",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "The index in the _string_ marking the start of the portion to parse. The default is 0.",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: `The index in the _string_ marking the end of the portion to parse.
The default is nil or the length of the _string_.`,
				},
				{
					Name: "radix",
					Type: "fixnum",
					Text: `The radix or base for the parsing which must be between 2 and 36 inclusive.
The default is 10.`,
				},
				{
					Name: "junk-allowed",
					Type: "boolean",
					Text: `If true parsing will not panic when encountering a non-numberic character
but instead just stop parsing.`,
				},
			},
			Return: "integer",
			Text:   `__parse-integer__ parses a string into an integer .`,
			Examples: []string{
				`(parse-integer " 123 ") => 123, 5`,
				`(parse-integer " 123x" :radix 4 :junk-allowed t) => 27, 4`,
			},
		}, &slip.CLPkg)
}

// ParseInteger represents the parse-integer function.
type ParseInteger struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ParseInteger) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 9)
	var (
		ra    []rune
		start int
		radix uint64 = 10
		junk  bool
	)
	if so, ok := args[0].(slip.String); ok {
		ra = []rune(so)
	} else {
		slip.TypePanic(s, depth, "string", args[0], "string")
	}
	end := len(ra)
	for pos := 1; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			slip.ErrorPanic(s, depth, "%s missing an argument", sym)
		}
		var n slip.Fixnum
		switch strings.ToLower(string(sym)) {
		case ":start":
			if n, ok = args[pos+1].(slip.Fixnum); ok {
				start = int(n)
			} else {
				slip.TypePanic(s, depth, string(sym), args[pos+1], "fixnum")
			}
		case ":end":
			if n, ok = args[pos+1].(slip.Fixnum); ok {
				end = int(n)
			} else {
				slip.TypePanic(s, depth, string(sym), args[pos+1], "fixnum")
			}
		case ":radix":
			if n, ok = args[pos+1].(slip.Fixnum); ok && 2 <= n && n <= 36 {
				radix = uint64(n)
			} else {
				slip.TypePanic(s, depth, string(sym), args[pos+1], "fixnum (2 to 36)")
			}
		case ":junk-allowed":
			junk = args[pos+1] != nil
		default:
			slip.TypePanic(s, depth, "keyword", sym, ":start", ":end", ":radix", ":junk-allowed")
		}
	}
	if end < start || len(ra) < end || start < 0 {
		slip.ErrorPanic(s, depth, "start and end of %d, %d are not valid for a string of length %d", start, end, len(ra))
	}
	// strconv.ParseInt can not be used as it does not report the end position
	// and does not handle junk other than returning an error.
	ra = ra[start:end]

	var (
		num      uint64
		pos      int
		r        rune
		neg      bool
		overflow bool
		space    bool
	)
	p0 := -1
done:
	for pos, r = range ra {
		if 0x7a < r {
			if !junk {
				slip.ErrorPanic(s, depth, "junk in string %q at %d", string(ra), start+pos)
			}
			pos--
			break
		}
		x := intBaseMap[r]
		if 0 <= p0 {
			if x == '_' {
				space = true
				continue
			}
			if radix <= uint64(x) || space {
				if !junk {
					slip.ErrorPanic(s, depth, "junk in string %q at %d", string(ra), start+pos)
				}
				pos--
				break
			}
			if overflow {
				continue
			}
			if (math.MaxInt64-uint64(x))/radix < num {
				overflow = true
			} else {
				num = num*radix + uint64(x)
			}
			continue
		}
		switch x {
		case '+':
			p0 = pos
		case '-':
			p0 = pos
			neg = true
		case '_':
			// space
		case '.':
			if !junk {
				slip.ErrorPanic(s, depth, "no non-whitespace in string %q", string(ra))
			}
			return slip.Values{nil, slip.Fixnum(start + pos)}
		default:
			if radix <= uint64(x) {
				if !junk {
					slip.ErrorPanic(s, depth, "junk in string %q at %d", string(ra), start+pos)
				}
				pos--
				break done
			}
			p0 = pos
			num = uint64(x)
		}
	}
	if overflow {
		var z big.Int
		bi, _ := z.SetString(string(ra[p0:pos+1]), int(radix))
		return slip.Values{(*slip.Bignum)(bi), slip.Fixnum(start + pos + 1)}
	}
	if neg {
		num = -num
	}
	if p0 < 0 {
		return slip.Values{nil, slip.Fixnum(start + pos + 1)}
	}
	return slip.Values{slip.Fixnum(num), slip.Fixnum(start + pos + 1)}
}
