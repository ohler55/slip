// Copyright (c) 2023, Peter Ohler, All rights reserved.

package xml

import (
	"bytes"
	"encoding/xml"
	"errors"
	"io"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Read{Function: slip.Function{Name: "xml-read", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "xml-read",
			Args: []*slip.DocArg{
				{
					Name: "input",
					Type: "input-stream|string",
					Text: "The stream or string to read from.",
				},
				{Name: "&key"},
				{
					Name: "strict",
					Type: "boolean",
					Text: "If non-nil parsing will be strict so will handle invalid XML better. (default is _t_)",
				},
				{
					Name: "html",
					Type: "boolean",
					Text: "If non-nil parsing will handle HTML. (default is _nil_)",
				},
				{
					Name: "trim",
					Type: "boolean",
					Text: "If non-nil parsing will trim whitespace from text and remove empty text. (default is _t_)",
				},
			},
			Return: "list",
			Text:   `__xml-read__ reads a XML from _input_.`,
			Examples: []string{
				`(xml-read "<?xml version="1.0"?>
<!DOCTYPE sample PUBLIC "sample.dtd">
<top id="123">
  <child>Some text.</child>
  <!--a comment-->
  <blank/>
</top>
") => ((:processing-instruction "xml" "version="1.0"")
 (:directive "DOCTYPE sample PUBLIC "sample.dtd"")
 ("top" (("id" . "123")) ("child" () "Some text.") (:comment "a comment")
        ("blank" ())))`,
			},
		}, &Pkg)
}

// Read represents the xml-read function.
type Read struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Read) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 7)
	var ir io.Reader
	switch ta := args[0].(type) {
	case slip.String:
		ir = strings.NewReader(string(ta))
	case io.Reader:
		ir = ta
	default:
		slip.TypePanic(s, depth, "input", args[0], "string", "input-stream")
	}
	dec := xml.NewDecoder(ir)
	trim := true
	for pos := 1; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			slip.NewPanic("%s missing an argument", sym)
		}
		switch string(sym) {
		case ":strict":
			dec.Strict = args[pos+1] != nil
		case ":html":
			if args[pos+1] != nil {
				dec.Strict = false
				dec.AutoClose = xml.HTMLAutoClose
				dec.Entity = xml.HTMLEntity
			}
		case ":trim":
			trim = args[pos+1] != nil
		default:
			slip.TypePanic(s, depth, "keyword", args[pos], ":strict", ":trim")
		}
	}
	var (
		stack   []slip.List
		element slip.List
	)
out:
	for {
		token, err := dec.Token()
		switch tt := token.(type) {
		case nil:
			if errors.Is(err, io.EOF) {
				break out
			}
			panic(err)
		case xml.StartElement:
			stack = append(stack, element)
			attrs := make(slip.List, len(tt.Attr))
			for i := 0; i < len(tt.Attr); i++ {
				attrs[i] = slip.List{
					slip.String(tt.Attr[i].Name.Local),
					slip.Tail{Value: slip.String(tt.Attr[i].Value)},
				}
			}
			element = slip.List{slip.String(tt.Name.Local), attrs}
		case xml.EndElement:
			stack[len(stack)-1] = append(stack[len(stack)-1], element)
			element = stack[len(stack)-1]
			stack[len(stack)-1] = nil
			stack = stack[:len(stack)-1]
		case xml.CharData:
			if trim {
				tt = bytes.TrimSpace(tt)
			}
			if 0 < len(tt) {
				element = append(element, slip.String(tt))
			}
		case xml.Comment:
			element = append(element, slip.List{slip.Symbol(":comment"), slip.String(tt)})
		case xml.Directive:
			element = append(element, slip.List{slip.Symbol(":directive"), slip.String(tt)})
		case xml.ProcInst:
			element = append(element, slip.List{
				slip.Symbol(":processing-instruction"),
				slip.String(tt.Target),
				slip.String(tt.Inst),
			})
		}
	}
	if len(element) == 1 {
		if list, ok := element[0].(slip.List); ok {
			element = list
		}
	}
	return element
}
