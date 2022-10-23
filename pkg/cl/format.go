// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Format{Function: slip.Function{Name: "format", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "format",
			Args: []*slip.DocArg{
				{
					Name: "destination",
					Type: "output-stream|string|t|nil",
					Text: `The destination to write to. If _t_ then write to _*standard-output*.
If _nil_ then return a string.`,
				},
				{
					Name: "control",
					Type: "string",
					Text: `The control string for the formatting.`,
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to be used as arguments to the _control_ string.",
				},
			},
			Return: "string|nil",
			Text: `__format__ output as directed by the _control_ string and write to the _destination_.
If _destination_ is _nil_ then a string is returned otherwise _nil_ is returned.


A control directive begins a _~_ character followed by control characters. A directive has the form of:

   ~[_modifier_],[_parameter_,]...<directive>


Common LISP defines a language for handling a variety of control directive operations. A subset of
those operations are implemented. An addition operator for inline evaluation of code is included as well.


Control directives and operations are:

 ___Directive___   ___Description___
 _          _  _                                                               _
 __~<newline>__  The _newline_ character and any following whitespace are ignored.
             The : modifier indicates only the _newline_ is ignored. The @
             modifier indicates the _newline_ is not ignored but any
             trailing whitespace is ignored.

 __~%__          Outputs a _newline_ character. A positive integer parameter
             indicates the number of _newline_ characters to output. The
             general form is ~_n_%.

 __~&__          Outputs a _newline_ if the previous output character was not a
             _newline_. A positive integer parameter indicates the number of
             _newline_ characters that should be output. The general form
             is ~_n_%.
`,
			Examples: []string{
				`(format nil "number: ~A" 123) => "number: 123"`,
			},
		}, &slip.CLPkg)
}

// Format represents the format function.
type Format struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Format) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 2 {
		slip.PanicArgCount(f, 2, -1)
	}

	// TBD

	return nil
}
