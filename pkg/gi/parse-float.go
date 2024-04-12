// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"bytes"
	"math/big"
	"strconv"

	"github.com/ohler55/slip"
)

// An approximation for converting base 10 to base 2 precision since
// big.Float uses base 10 precision for formatting and base 2 for parsing.
const prec10t2 = 3.32

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ParseFloat{Function: slip.Function{Name: "parse-float", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parse-float",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The string to read from.",
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
					Name: "type",
					Type: "symbol",
					Text: `The float return type. Valid values are _short-float_, _single_float_,
_double-float_, or _long-float_. The default is _double-float_.`,
				},
			},
			Return: "float",
			Text:   `__parse-float__ reads a _float_ from _string_ and returns the value read.`,
			Examples: []string{
				`(parse-float "1.23 ") => 1.23`,
			},
		}, &Pkg)
}

// ParseFloat represents the parse-float function.
type ParseFloat struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ParseFloat) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 7)
	ss, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	var (
		typ slip.Object
		buf []byte
	)
	start := 0
	end := -1
	if 1 < len(args) {
		ra := []rune(ss)
		var sym slip.Symbol

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
				case slip.Symbol(":type"):
					typ = args[pos]
				default:
					slip.PanicType("keyword", sym, ":start", ":end", ":type")
				}
			} else {
				slip.PanicType("keyword", sym, ":start", ":end", ":type")
			}
		}
		if end < 0 {
			end = len(ra)
		}
		if start < 0 || len(ra) <= start || end < 0 || len(ra) < end || end < start {
			slip.NewPanic("the bounding indices %d and %d are not valid for string of length %d",
				start, end, len(ra))
		}
		ra = ra[start:end]
		buf = []byte(string(ra))
	} else {
		buf = []byte(ss)
	}
	buf = bytes.TrimSpace(buf)
	type expType struct {
		typ slip.Symbol
		x   byte
	}
	for _, xt := range []expType{
		{x: 'd', typ: slip.DoubleFloatSymbol},
		{x: 's', typ: slip.SingleFloatSymbol},
		{x: 'f', typ: slip.SingleFloatSymbol},
		{x: 'l', typ: slip.LongFloatSymbol},
		{x: 'D', typ: slip.DoubleFloatSymbol},
		{x: 'S', typ: slip.SingleFloatSymbol},
		{x: 'F', typ: slip.SingleFloatSymbol},
		{x: 'L', typ: slip.LongFloatSymbol},
	} {
		if i := bytes.IndexByte(buf, xt.x); 0 <= i {
			buf[i] = 'e'
			if typ == nil {
				typ = xt.typ
			}
			break
		}
	}
	if typ == slip.LongFloatSymbol {
		cnt := len(buf) - 2 - bytes.Count(buf, []byte{'-'}) - bytes.Count(buf, []byte{'+'})
		if num, _, err := big.ParseFloat(string(buf), 10, uint(prec10t2*float64(cnt)), big.AwayFromZero); err == nil {
			result = (*slip.LongFloat)(num)
		}
	} else {
		f64, err := strconv.ParseFloat(string(buf), 64)
		if err != nil {
			slip.NewPanic("parsing float failed: %s", err)
		}
		switch typ {
		case nil, slip.DoubleFloatSymbol:
			result = slip.DoubleFloat(f64)
		case slip.SingleFloatSymbol, slip.ShortFloatSymbol:
			result = slip.SingleFloat(f64)
		}
	}
	return
}
