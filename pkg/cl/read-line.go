// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"errors"
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReadLine{Function: slip.Function{Name: "read-line", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "read-line",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "stream",
					Type: "input-stream",
					Text: "The stream to read from. The default is _*standard-input*_",
				},
				{
					Name: "eof-error-p",
					Type: "boolean",
					Text: `If true an EOF error is raised when a read attempt is made at
the end of the stream. The default is _t_.`,
				},
				{
					Name: "eof-value",
					Type: "object",
					Text: "The value to return on EOF if _eof-error-p_ is nil.",
				},
				{
					Name: "recursive-p",
					Type: "boolean",
					Text: "If true the call is embedded in a higher level function. Has no impact on behavior.",
				},
			},
			Return: "string,bool",
			Text: `__read-line__ reads a line from the _stream_ and returns the line as a _string_ and
a missing-newline indicator. The newline is not included in the returned string. The missing-newline
indicator is only true at the end of the stream or file.`,
			Examples: []string{
				`(read-line (make-string-input-stream (format nil "abc~%def"))) => "abc", nil`,
				`(read-line (make-string-input-stream "abc")) => "abc", t`,
			},
		}, &slip.CLPkg)
}

// ReadLine represents the read-line function.
type ReadLine struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ReadLine) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 4)
	is := s.Get(slip.Symbol("*standard-input*"))
	if 0 < len(args) {
		is = args[0]
	}
	rr, ok := is.(io.RuneReader)
	if !ok {
		slip.TypePanic(s, depth, "stream", args[0], "input-stream")
	}
	// Not at all efficient but it's the best that can be done with a buffered
	// input.
	var (
		ra  []rune
		eof slip.Object
	)
	for {
		r, _, err := rr.ReadRune()
		if err != nil {
			if errors.Is(err, io.EOF) {
				if 0 < len(ra) {
					eof = slip.True
					break
				}
				if 1 < len(args) && args[1] == nil {
					var result slip.Object
					if 2 < len(args) {
						result = args[2]
					}
					return slip.Values{result, slip.True}
				}
				ss, _ := is.(slip.Stream)
				slip.StreamPanic(s, depth, ss, "read failed. %s", err)
			}
		}
		if r == '\n' {
			break
		}
		ra = append(ra, r)
	}
	return slip.Values{slip.String(ra), eof}
}
