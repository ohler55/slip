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
			f := WriteRow{Function: slip.Function{Name: "csv-write-row", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "csv-write-row",
			Args: []*slip.DocArg{
				{
					Name: "row",
					Type: "list",
					Text: "The row to write.",
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
			Text:   `__csv-write-row__ writes a CSV to _output_. TBD`,
			Examples: []string{
				`(csv-write-row '(A B)) nil) => "A,B\n"`,
			},
		}, &Pkg)
}

// WriteRow represents the csv-write-row function.
type WriteRow struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WriteRow) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 6)
	row, ok := args[0].(slip.List)
	if !ok {
		slip.TypePanic(s, depth, "row", args[0], "list")
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
	csvWriteRow(s, cw, row, rec, depth)
	cw.Flush()
	var sb *strings.Builder
	if sb, ok = w.(*strings.Builder); ok {
		return slip.String(sb.String())
	}
	return nil
}

func csvWriteRow(s *slip.Scope, cw *csv.Writer, row slip.Object, rec []string, depth int) {
	list, ok := row.(slip.List)
	if !ok {
		slip.TypePanic(s, depth, "row", row, "list")
	}
	rec = rec[:0]
	for _, v := range list {
		switch tv := v.(type) {
		case nil:
			rec = append(rec, "")
		case slip.String:
			rec = append(rec, string(tv))
		default:
			rec = append(rec, slip.ObjectString(v)) // TBD string with no quotes and nil as empty string
		}
	}
	if err := cw.Write(rec); err != nil {
		panic(err)
	}
}
