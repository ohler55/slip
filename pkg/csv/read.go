// Copyright (c) 2023, Peter Ohler, All rights reserved.

package csv

import (
	"encoding/csv"
	"errors"
	"io"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Read{Function: slip.Function{Name: "csv-read", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "csv-read",
			Args: []*slip.DocArg{
				{
					Name: "input",
					Type: "input-stream|string",
					Text: "The stream or string to read from.",
				},
				{Name: "&key"},
				{
					Name: "separator",
					Type: "character",
					Text: "The separator character in the CSV. default: #\\,",
				},
				{
					Name: "comment-char",
					Type: "character",
					Text: "The comment character in the CSV. default: _nil_",
				},
				{
					Name: "lazy-quotes",
					Type: "boolean",
					Text: `If non-nil strict quoting is enforced otherwise quotes can appear in a field.
default: false`,
				},
				{
					Name: "trim",
					Type: "boolean",
					Text: "If non-nil whitespace is not removed otherwise it is. default: false.",
				},
			},
			Return: "list",
			Text:   `__csv-read__ reads a CSV from _input_.`,
			Examples: []string{
				`(csv-read "
first second
1,2
3,4
") => (("first" "second") (1 2) (3 4))`,
			},
		}, &Pkg)
}

// Read represents the csv-read function.
type Read struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Read) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 9)
	var ir io.Reader
	switch ta := args[0].(type) {
	case slip.String:
		ir = strings.NewReader(string(ta))
	case io.Reader:
		ir = ta
	default:
		slip.TypePanic(s, depth, "input", args[0], "string", "input-stream")
	}
	cr := csv.NewReader(ir)
	cr.ReuseRecord = true
	_ = csvSetReaderOptions(s, cr, args[1:], false, depth)
	var list slip.List
	for {
		rec, err := cr.Read()
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			panic(err)
		}
		row := make(slip.List, len(rec))
		for i, str := range rec {
			if cr.TrimLeadingSpace {
				row[i] = slip.String(strings.TrimSpace(str))
			} else {
				row[i] = slip.String(str)
			}
		}
		list = append(list, row)
	}
	return list
}

func csvSetReaderOptions(s *slip.Scope, cr *csv.Reader, args slip.List, bagOk bool, depth int) (bagIt bool) {
	for pos := 0; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			slip.NewPanic("%s missing an argument", sym)
		}
		switch strings.ToLower(string(sym)) {
		case ":separator":
			if c, ok := args[pos+1].(slip.Character); ok {
				cr.Comma = rune(c)
			} else {
				slip.TypePanic(s, depth, string(sym), args[pos+1], "character")
			}
		case ":comment-char":
			if c, ok := args[pos+1].(slip.Character); ok {
				cr.Comment = rune(c)
			} else {
				slip.TypePanic(s, depth, string(sym), args[pos+1], "character")
			}
		case ":lazy-quotes":
			cr.LazyQuotes = args[pos+1] != nil
		case ":trim":
			cr.TrimLeadingSpace = args[pos+1] != nil
		case ":as-bag":
			if bagOk {
				bagIt = args[pos+1] != nil
			} else {
				slip.TypePanic(s, depth, "keyword", sym, ":separator", ":comment-char", ":lazy-quotes", ":trim")
			}
		default:
			if bagOk {
				slip.TypePanic(s, depth, "keyword", sym, ":separator", ":comment-char", ":lazy-quotes", ":trim", ":as-bag")
			}
			slip.TypePanic(s, depth, "keyword", sym, ":separator", ":comment-char", ":lazy-quotes", ":trim")
		}
	}
	return
}
