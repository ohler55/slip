// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := YesOrNoP{Function: slip.Function{Name: "yes-or-no-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "yes-or-no-p",
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
			Text: `__yes-or-no-p__ asks a question using the format _control_ and the _arguments_ if provided.
The response on *standard-input* of either 'yes' or 'no' is then returned as a boolean value. A values not
recognized will cause the user to be prompted again.`,
			Examples: []string{
				`(yes-or-no-p) => t ;; if user types yes after a prompt`,
			},
		}, &slip.CLPkg)
}

// YesOrNoP represents the yes-or-no-p function.
type YesOrNoP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *YesOrNoP) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	var prompt []byte
	if 0 < len(args) {
		prompt = FormatArgs(s, args)
		prompt = append(prompt, ' ')
	}
	prompt = append(prompt, "(yes or no) "...)
	w := s.Get("*standard-output*").(io.Writer)
	r := s.Get("*standard-input*").(io.Reader)

top:
	for {
		if _, err := w.Write(prompt); err != nil {
			slip.PanicStream(w.(slip.Stream), "%s", err)
		}
		answer := readLine(r)
		switch strings.ToLower(string(answer)) {
		case "yes":
			result = slip.True
			break top
		case "no":
			break top
		default:
			_, _ = w.Write([]byte("Please type \"yes\" for yes or \"no\" for no.\n"))
			goto top
		}
	}
	_, _ = w.Write([]byte{'\n'})

	return
}
