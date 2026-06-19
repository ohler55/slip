// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"io"
	"os"

	"github.com/ohler55/slip"
)

func defCoverageReport() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CoverageReport{Function: slip.Function{Name: "coverage-report", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "coverage-report",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "destination",
					Type: "output-stream|t|nil|string",
					Text: `The destination to write to. If _t_ then write to _*standard-output*_.
If _nil_ then return a string. If a string then assume it is a filepath and create a file and
write to that file. Finally, if an output-stream, write to the stream.`,
				},
			},
			Return: "nil|string",
			Text: `__coverage-report__ generates a coverage report and writes to _destination_.
If _destination_ is __t__ then write output to _*standard-output*_. If _destination_ is __nil__
then return the report as a __string__. If a _destination_ is a string then assume it is a filepath
and create a file and write to that file. Finally, if an __output-stream__, write to the stream.`,
			Examples: []string{
				`(coverage-report t) => nil`,
			},
		}, &Pkg)
}

// CoverageReport represents the coverage-report function.
type CoverageReport struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CoverageReport) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 1)

	var a0 slip.Object = slip.True
	if 0 < len(args) {
		a0 = args[0]
	}
	var (
		w  io.Writer
		ss slip.Stream
	)
	switch ta := a0.(type) {
	case nil:
		// leave w as nil
	case io.Writer:
		w = ta
		ss, _ = args[0].(slip.Stream)
	case slip.String:
		gf, err := os.Create(string(ta))
		if err != nil {
			slip.FilePanic(s, depth, ta, "create file failed: %s", err)
		}
		w = gf
		ss = (*slip.FileStream)(gf)
		defer func() { _ = gf.Close() }()
	default:
		if ta == slip.True {
			so := s.Get("*standard-output*")
			ss, _ = so.(slip.Stream)
			if w, _ = so.(io.Writer); w != nil {
				break
			}
		}
		slip.TypePanic(s, depth, "destination", ta, "output-stream")
	}
	report := slip.CoverageReport(nil)
	if w == nil {
		return slip.String(report)
	}
	if _, err := w.Write(report); err != nil {
		slip.StreamPanic(s, depth, ss, "write failed. %s", err)
	}
	return nil
}
