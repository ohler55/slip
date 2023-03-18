// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"
	"math"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Write{Function: slip.Function{Name: "write", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "write",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The _object_ to be written.",
				},
				{Name: "&key"},
				{Name: "array", Type: "boolean"},
				{Name: "base", Type: "fixnum"},
				{Name: "case", Type: ":upcase :downcase :capitalize"},
				{Name: "circle", Type: "boolean"},
				{Name: "escape", Type: "boolean"},
				{Name: "gensym", Type: "boolean"},
				{Name: "length", Type: "fixnum or nil"},
				{Name: "level", Type: "fixnum or nil"},
				{Name: "lines", Type: "fixnum or nil"},
				{Name: "miser-width", Type: "fixnum or nil"},
				{Name: "pretty", Type: "boolean"},
				{Name: "radix", Type: "boolean"},
				{Name: "readably", Type: "boolean"},
				{Name: "right-margin", Type: "fixnum or nil"},
				{
					Name: "stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Return: "object",
			Text: `__write__ a string representation of the _object_ to the provided stream.
Output is produced as if according to the default print variables unless overridden by the keyword parameters.

  __Parameter__        __Corresponding Dynamic Variable__
  array            _*print-array*_
  base             _*print-base*_
  case             _*print-case*_
  circle           _*print-circle*_
  escape           _*print-escape*_
  gensym           _*print-gensym*_
  length           _*print-length*_
  level            _*print-level*_
  lines            _*print-lines*_
  miser-width      _*print-miser-width*_
  pretty           _*print-pretty*_
  radix            _*print-radix*_
  readably         _*print-readably*_
  right-margin     _*print-right-margin*_


If the _output-stream_ is not provided then the _*standard-output*_ is used. The _object_ is returned.`,
			Examples: []string{
				"(write 123) => 123 ;; 123 is written",
			},
		}, &slip.CLPkg)
}

// Write represents the write function.
type Write struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Write) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 33 < len(args) {
		slip.PanicArgCount(f, 1, 33)
	}
	p := *slip.DefaultPrinter()

	obj := args[0]
	var w io.Writer = slip.StandardOutput.(io.Writer)

	for i := 1; i < len(args)-1; i += 2 {
		sym, ok := args[i].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[i], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":array":
			p.Array = args[i+1] != nil
		case ":base":
			if base, ok := args[i+1].(slip.Fixnum); ok && 2 <= base && base <= 36 {
				p.Base = uint(base)
			} else {
				slip.PanicType(":base", args[i+1], "fixnum between 2 and 36 inclusive")
			}
		case ":case":
			if args[i+1] == nil {
				p.Case = nil
			} else {
				key, _ := args[i+1].(slip.Symbol)
				key = slip.Symbol(strings.ToLower(string(key)))
				switch key {
				case slip.Symbol(":upcase"), slip.Symbol(":downcase"), slip.Symbol(":capitalize"):
					p.Case = key
				default:
					slip.PanicType(":case", args[i+1], ":downcase", ":upcase", ":capitalize")
				}
			}
		case ":circle":
			p.Circle = args[i+1] != nil
		case ":escape":
			p.Escape = args[i+1] != nil
		case ":gensym":
			p.Gensym = args[i+1] != nil
		case ":length":
			p.Length = f.asUint(keyword, args[i+1])
		case ":level":
			p.Level = f.asUint(keyword, args[i+1])
		case ":lines":
			p.Lines = f.asUint(keyword, args[i+1])
		case ":miser-width":
			p.MiserWidth = f.asUint(keyword, args[i+1])
		case ":pretty":
			p.Pretty = args[i+1] != nil
		case ":radix":
			p.Radix = args[i+1] != nil
		case ":readably":
			p.Readably = args[i+1] != nil
		case ":right-margin":
			p.RightMargin = f.asUint(keyword, args[i+1])
		case ":stream":
			if w, ok = args[i+1].(io.Writer); !ok {
				slip.PanicType(":stream", args[i+1], "output-stream")
			}
		default:
			slip.PanicType("keyword", sym,
				":array", ":base", ":case", ":circle", ":escape", ":gensym", ":length", ":level", ":lines",
				":miser-width", ":pretty", ":radix", ":readably", ":right-margin", ":stream")
		}
	}
	b := p.Append([]byte{}, obj, 0)
	if _, err := w.Write(b); err != nil {
		panic(err)
	}
	return obj
}

func (f *Write) asUint(keyword string, a slip.Object) uint {
	if a == nil {
		return math.MaxInt
	}
	num, ok := a.(slip.Fixnum)
	if !ok || num < 0 {
		slip.PanicType(keyword, a, "non-negative fixnum")
	}
	return uint(num)
}
