// Copyright (c) 2023, Peter Ohler, All rights reserved.

package csv

import (
	"encoding/csv"
	"errors"
	"io"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/gi"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Iterate{Function: slip.Function{Name: "csv-iterate", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "csv-iterate",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to call for each row in _input_.",
				},
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
				{
					Name: "as-bag",
					Type: "boolean",
					Text: `If non-nil an instance of the _bag-flavor_ is created with key-value pairs
of the header column name and the value in the row. A header is expected.`,
				},
			},
			Return: "list",
			Text:   `__csv-iterate__ reads a CSV from _input_.`,
			Examples: []string{
				`(csv-iterate "
1,2
3,4
" (lambda (row) (format t "*~A" row)) => nil ;; prints *(1 2)*(3 4)`,
			},
		}, &slip.CLPkg)
}

// Iterate represents the csv-iterate function.
type Iterate struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Iterate) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 12)
	d2 := depth + 1
	var caller slip.Caller
	channel, ok := args[0].(gi.Channel)
	if !ok {
		caller = cl.ResolveToCaller(s, args[0], d2)
	}
	var ir io.Reader
	switch ta := args[1].(type) {
	case slip.String:
		ir = strings.NewReader(string(ta))
	case io.Reader:
		ir = ta
	default:
		slip.PanicType("input", ta, "string", "input-stream")
	}
	cr := csv.NewReader(ir)
	cr.ReuseRecord = false
	bagIt := csvSetReaderOptions(cr, args[2:], true)
	var header []string
	for {
		rec, err := cr.Read()
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			panic(err)
		}
		if bagIt {
			if header == nil {
				header = rec
				continue
			}
			bm := map[string]any{}
			for i, k := range header {
				if i < len(rec) {
					bm[k] = rec[i]
				}
			}
			inst := bag.Flavor().MakeInstance().(*flavors.Instance)
			inst.Any = bm
			if caller != nil {
				_ = caller.Call(s, slip.List{inst}, d2)
			} else {
				channel <- inst
			}
		} else {
			row := make(slip.List, len(rec))
			for i, str := range rec {
				if cr.TrimLeadingSpace {
					row[i] = slip.String(strings.TrimSpace(str))
				} else {
					row[i] = slip.String(str)
				}
			}
			if caller != nil {
				_ = caller.Call(s, slip.List{row}, d2)
			} else {
				channel <- row
			}
		}
	}
	return nil
}
