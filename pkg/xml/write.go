// Copyright (c) 2023, Peter Ohler, All rights reserved.

package xml

import (
	"encoding/xml"
	"io"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Write{Function: slip.Function{Name: "xml-write", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "xml-write",
			Args: []*slip.DocArg{
				{
					Name: "data",
					Type: "list",
					Text: "The data to write as XML.",
				},
				{Name: "&optional"},
				{
					Name: "output",
					Type: "output-stream|t|nil",
					Text: "The stream to write to.",
				},
				{Name: "&key"},
				{
					Name: "indent",
					Type: "string",
					Text: "The indentation for each element of the XML repeated by the depth.",
				},
			},
			Return: "nil|string",
			Text: `__xml-write__ writes an XML to _output_. The contents of _data_ must
match the format of what would be produced by __xml-read__.`,
			Examples: []string{
				`(xml-write '( nil) => "TBD"`,
			},
		}, &Pkg)
}

// Write represents the xml-write function.
type Write struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Write) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 4)
	data, ok := args[0].(slip.List)
	if !ok || len(data) < 1 {
		slip.TypePanic(s, depth, "data", args[0], "list")
	}
	w := s.Get("*standard-output*").(io.Writer)
	if 1 < len(args) {
		switch ta := args[1].(type) {
		case nil:
			w = &strings.Builder{}
		case io.Writer:
			w = ta
		case slip.Symbol:
			// Most likely a keyword so just keep going.
		default:
			if ta != slip.True {
				slip.TypePanic(s, depth, "output", ta, "output-stream", "t", "nil")
			}
		}
	}
	enc := xml.NewEncoder(w)
	for pos := 2; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			slip.ErrorPanic(s, depth, "%s missing an argument", sym)
		}
		if string(sym) == ":indent" {
			var ss slip.String
			if ss, ok = args[pos+1].(slip.String); ok {
				enc.Indent("", string(ss))
			} else {
				slip.TypePanic(s, depth, ":indent", args[pos+1], "string")
			}
		} else {
			slip.TypePanic(s, depth, "keyword", args[pos], ":indent")
		}
	}
	// The top level list can be a list of elements without a name. Determine
	// that by looking at the first element of the list.
	if _, ok := data[0].(slip.List); ok {
		for _, child := range data {
			f.encode(s, enc, child, depth)
		}
	} else {
		f.encode(s, enc, data, depth)
	}
	if err := enc.Flush(); err != nil {
		panic(err)
	}
	var sb *strings.Builder
	if sb, ok = w.(*strings.Builder); ok {
		return slip.String(sb.String())
	}
	return nil
}

func (f *Write) encode(s *slip.Scope, enc *xml.Encoder, data slip.Object, depth int) {
	switch td := data.(type) {
	case slip.List: // all except text
		if len(td) < 2 {
			slip.ErrorPanic(s, depth, "failed to convert to XML due to unexpected list content: %s", td)
		}
		switch td[0] {
		case slip.Symbol(":comment"):
			if ss, ok := td[1].(slip.String); ok {
				if err := enc.EncodeToken(xml.Comment(ss)); err != nil {
					panic(err)
				}
			} else {
				slip.TypePanic(s, depth, "comment", td[1], "string")
			}
		case slip.Symbol(":directive"):
			if ss, ok := td[1].(slip.String); ok {
				if err := enc.EncodeToken(xml.Directive(ss)); err != nil {
					panic(err)
				}
			} else {
				slip.TypePanic(s, depth, "directive", td[1], "string")
			}
		case slip.Symbol(":processing-instruction"):
			var pi xml.ProcInst
			if ss, ok := td[1].(slip.String); ok {
				pi.Target = string(ss)
			} else {
				slip.TypePanic(s, depth, "processing instruction target", td[1], "string")
			}
			if 2 < len(td) {
				if ss, ok := td[2].(slip.String); ok {
					pi.Inst = []byte(ss)
				} else {
					slip.TypePanic(s, depth, "processing instruction", td[2], "string")
				}
			}
			if err := enc.EncodeToken(pi); err != nil {
				panic(err)
			}
		default:
			var se xml.StartElement
			switch t0 := td[0].(type) {
			case slip.Symbol:
				se.Name.Local = string(t0)
			case slip.String:
				se.Name.Local = string(t0)
			default:
				slip.TypePanic(s, depth, "element name", t0, "string", "symbol")
			}
			switch t1 := td[1].(type) {
			case nil:
				// nothing to add
			case slip.List:
				for _, e := range t1 {
					if a, _ := e.(slip.List); len(a) == 2 {
						var attr xml.Attr
						switch ta := a[0].(type) {
						case slip.Symbol:
							attr.Name.Local = string(ta)
						case slip.String:
							attr.Name.Local = string(ta)
						default:
							slip.TypePanic(s, depth, "element attribute name", ta, "string", "symbol")
						}
						v := a[1]
					value:
						switch tv := v.(type) {
						case slip.Symbol:
							attr.Value = string(tv)
						case slip.String:
							attr.Value = string(tv)
						case slip.Tail:
							v = tv.Value
							goto value
						default:
							slip.TypePanic(s, depth, "element attribute value", tv, "string", "symbol")
						}
						se.Attr = append(se.Attr, attr)
					} else {
						slip.TypePanic(s, depth, "element attribute", e, "cons")
					}
				}
			default:
				slip.TypePanic(s, depth, "element attributes", t1, "assoc list")
			}
			if err := enc.EncodeToken(se); err != nil {
				panic(err)
			}
			for _, child := range td[2:] {
				f.encode(s, enc, child, depth)
			}
			if err := enc.EncodeToken(xml.EndElement{Name: se.Name}); err != nil {
				panic(err)
			}
		}
	case slip.String:
		if err := enc.EncodeToken(xml.CharData(td)); err != nil {
			panic(err)
		}
	default:
		slip.ErrorPanic(s, depth, "failed to convert to XML due to unexpected list content: %s", td)
	}
}
