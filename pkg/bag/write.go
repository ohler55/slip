// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"io"

	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Write{Function: slip.Function{Name: "bag-write", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-write",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to write.",
				},
				{Name: slip.AmpOptional},
				{
					Name: "stream",
					Type: "t|nil|output-stream",
					Text: "Output stream. Default: nil (return a string).",
				},
				{Name: slip.AmpKey},
				{
					Name: "pretty",
					Type: "boolean",
					Text: `The value to use in place of the _*print-pretty*_ value. If _t_ then the
JSON or SEN output is indented according to the other keyword options.`,
				},
				{
					Name: "depth",
					Type: "integer",
					Text: "The maximum number of nested elements on a line in the output. Default: 4.",
				},
				{
					Name: "right-margin",
					Type: "integer",
					Text: "The value to use in place of the _*print-right-margin*_ value.",
				},
				{
					Name: "time-format",
					Type: "string",
					Text: "The value to use in place of the _*bag-time-format*_ value.",
				},
				{
					Name: "time-wrap",
					Type: "string",
					Text: "The value to use in place of the _*bag-time-wrap*_ value.",
				},
				{
					Name: "json",
					Type: "boolean",
					Text: "If true the output is JSON formatted otherwise output is SEN format.",
				},
				{
					Name: "color",
					Type: "boolean",
					Text: "If true the output is colorized.",
				},
			},
			Return: "nil|string",
			Text: `__bag-write__ writes a _bag_ to _*standard-output*_, a provided output _stream_,
or to a string that is returned. If _stream_ is _t_ then output is to _*standard-output*_. If
_stream_ is _nil_ then output is a returned string. Any other _stream_ value must be an output
stream which is where output is written to. Output can be either JSON or SEN format as defined
in the OjG package.

This is the same as the _:write_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it write a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`,
				`(bag-write bag) => "{a: 7}")`,
			},
		}, &slip.CLPkg)
}

// Write represents the write function.
type Write struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Write) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 16 < len(args) {
		slip.PanicArgCount(f, 1, 16)
	}
	pos := len(args) - 1
	obj, ok := args[pos].(*flavors.Instance)
	if !ok || obj.Flavor != flavor {
		slip.PanicType("bag", args[1], "bag")
	}
	return writeBag(obj, args, pos)
}

func writeBag(obj *flavors.Instance, args slip.List, pos int) (result slip.Object) {
	var out io.Writer
	if 0 < pos {
		pos--
		switch ta := args[pos].(type) {
		case nil:
			// leave as nil for output to string
			pos--
		case io.Writer:
			out = ta
			pos--
		case slip.Symbol:
			// probably a key or an error
		default:
			if ta == slip.True {
				out = slip.StandardOutput.(io.Writer)
				pos--
			} else {
				slip.PanicType("stream", ta, "nil", "t", "output-stream")
			}
		}
	}
	dp := slip.DefaultPrinter()
	pw := pretty.Writer{
		Options:  options,
		Width:    int(dp.RightMargin),
		MaxDepth: 4,
		SEN:      true,
	}
	pw.Indent = 2
	prty := dp.Pretty
	for ; 0 < pos; pos -= 2 {
		sym := args[pos].(slip.Symbol)
		switch string(sym) {
		case ":pretty":
			prty = args[pos-1] != nil
		case ":depth":
			num, ok := args[pos-1].(slip.Fixnum)
			if !ok {
				slip.PanicType(":depth", args[pos-1], "fixnum")
			}
			pw.MaxDepth = int(num)
			if pw.MaxDepth <= 0 {
				pw.Indent = 0
			}
		case ":right-margin":
			num, ok := args[pos-1].(slip.Fixnum)
			if !ok {
				slip.PanicType(":right-margin", args[pos-1], "fixnum")
			}
			pw.Width = int(num)
		case ":time-format":
			switch ta := args[pos-1].(type) {
			case nil:
				pw.TimeFormat = ""
			case slip.String:
				pw.TimeFormat = string(ta)
			default:
				slip.PanicType(":time-format", args[pos-1], "string")
			}
		case ":time-wrap":
			switch ta := args[pos-1].(type) {
			case nil:
				pw.TimeWrap = ""
			case slip.String:
				pw.TimeWrap = string(ta)
			default:
				slip.PanicType(":time-wrap", args[pos-1], "string")
			}
		case ":json":
			pw.SEN = args[pos-1] == nil
		case ":color":
			pw.Color = args[pos-1] != nil

		default:
			slip.PanicType("keyword", sym, ":pretty", ":depth", ":right-margin",
				":time-format", ":time-wrap", ":json", ":color")
		}
	}
	var b []byte
	switch {
	case prty && 1 < pw.MaxDepth:
		b = pw.Encode(obj.Any)
	case pw.SEN:
		b = sen.Bytes(obj.Any, &pw.Options)
	default:
		b = []byte(oj.JSON(obj.Any, &pw.Options))
	}
	if out == nil {
		return slip.String(b)
	}
	if _, err := out.Write(b); err != nil {
		panic(err)
	}
	return nil
}
