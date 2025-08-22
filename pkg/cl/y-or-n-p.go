// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := YOrNP{Function: slip.Function{Name: "y-or-n-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "y-or-n-p",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "control",
					Type: "string",
					Text: "A format control string.",
				},
				{Name: "&rest"},
				{
					Name: "arguments",
					Type: "object",
					Text: "Arguments for the control string.",
				},
			},
			Return: "boolean",
			Text: `__y-or-n-p__ asks a question using the format _control_ and the _arguments_ if provided.
The response on *standard-input* of either 'y' or 'n' is then returned as a boolean value. A values not
recognized will cause the user to be prompted again.`,
			Examples: []string{
				`(y-or-n-p) => t ;; if user types y after a prompt`,
			},
		}, &slip.CLPkg)
}

// YOrNP represents the y-or-n-p function.
type YOrNP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *YOrNP) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	var prompt []byte
	if 0 < len(args) {
		prompt = FormatArgs(s, args, depth)
		prompt = append(prompt, ' ')
	}
	prompt = append(prompt, "(y or n) "...)
	w := s.Get("*standard-output*").(io.Writer)
	r := s.Get("*standard-input*").(io.Reader)

top:
	for {
		if _, err := w.Write(prompt); err != nil {
			slip.PanicStream(w.(slip.Stream), "%s", err)
		}
		answer := readLine(r)
		switch string(answer) {
		case "y", "Y":
			result = slip.True
			break top
		case "n", "N":
			break top
		default:
			_, _ = w.Write([]byte("Please type \"y\" for yes or \"n\" for no.\n"))
			goto top
		}
	}
	_, _ = w.Write([]byte{'\n'})

	return
}

func readLine(r io.Reader) (line []byte) {
	b := []byte{0}
	for {
		if n, err := r.Read(b); err != nil || n != 1 {
			if err != nil {
				slip.PanicStream(r.(slip.Stream), "%s", err)
			}
		}
		if b[0] == '\n' || b[0] == '\r' {
			break
		}
		line = append(line, b[0])
	}
	return
}
