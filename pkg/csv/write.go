// Copyright (c) 2023, Peter Ohler, All rights reserved.

package csv

import (
	"encoding/csv"
	"io"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Write{Function: slip.Function{Name: "csv-write", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "csv-write",
			Args: []*slip.DocArg{
				{
					Name: "data",
					Type: "list",
					Text: "The data to write.",
				},
				{Name: "&optional"},
				{
					Name: "output",
					Type: "output-stream|t|nil",
					Text: "The stream to write to.",
				},
				{Name: "&key"},
				{
					Name: "separator",
					Type: "character",
					Text: "The separator character in the CSV. default: #\\,",
				},
				{
					Name: "crlf",
					Type: "boolean",
					Text: "If non-nil a return and linefeed are used as the row terminator. default: false.",
				},
			},
			Return: "nil|string",
			Text:   `__csv-write__ writes a CSV to _output_.`,
			Examples: []string{
				`(csv-write '((A B) (1 2) (3 4)) nil) => "A,B\n1,2\n3,4\n"`,
			},
		}, &Pkg)
}

// Write represents the csv-write function.
type Write struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Write) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 6)
	data, ok := args[0].(slip.List)
	if !ok {
		slip.TypePanic(s, depth, "data", args[0], "list")
	}
	w := s.Get("*standard-output*").(io.Writer)
	args = args[1:]
	if 0 < len(args) {
		switch ta := args[0].(type) {
		case nil:
			w = &strings.Builder{}
			args = args[1:]
		case io.Writer:
			w = ta
			args = args[1:]
		case slip.Symbol:
			// Most likely a keyword so just keep going.
		default:
			if ta == slip.True {
				args = args[1:]
			} else {
				slip.TypePanic(s, depth, "output", ta, "output-stream", "t", "nil")
			}
		}
	}
	cw := csv.NewWriter(w)
	csvSetWriterOptions(s, cw, args, depth)
	var rec []string
	for _, row := range data {
		csvWriteRow(s, cw, row, rec, depth)
	}
	cw.Flush()
	var sb *strings.Builder
	if sb, ok = w.(*strings.Builder); ok {
		return slip.String(sb.String())
	}
	return nil
}

func csvSetWriterOptions(s *slip.Scope, cw *csv.Writer, args slip.List, depth int) {
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
				cw.Comma = rune(c)
			} else {
				slip.TypePanic(s, depth, string(sym), args[pos+1], "character")
			}
		case ":crlf":
			cw.UseCRLF = args[pos+1] != nil
		default:
			slip.TypePanic(s, depth, "keyword", sym, ":separator", ":crlf")
		}
	}
}
