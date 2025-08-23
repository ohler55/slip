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
			f := PeekChar{Function: slip.Function{Name: "peek-char", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "peek-char",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "peek-type",
					Type: "character|t|nil",
					Text: "The type of next character to read.",
				},
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
			Return: "character",
			Text:   `__peek-char__ peeks at the next character in _stream_ without removing it.`,
			Examples: []string{
				`(peek-char (make-string-input-stream "abc")) => #\a`,
			},
		}, &slip.CLPkg)
}

// PeekChar represents the peek-char function.
type PeekChar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PeekChar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 5)
	is := s.Get(slip.Symbol("*standard-input*"))
	var (
		pt  slip.Object
		r   rune
		err error
	)
	if 0 < len(args) {
		pt = args[0]
		if 1 < len(args) {
			is = args[1]
		}
	}
	ss, _ := is.(slip.Stream)
	rs, ok := is.(io.RuneScanner)
	if !ok {
		slip.TypePanic(s, depth, "stream", args[1], "input-stream")
	}
	switch tpt := pt.(type) {
	case nil:
		r, _, err = rs.ReadRune()
	case slip.Character:
		for {
			r, _, err = rs.ReadRune()
			if r == rune(tpt) || err != nil {
				break
			}
		}
	default:
		if pt != slip.True {
			slip.TypePanic(s, depth, "peek-type", pt, "character", "t", "nil")
		}
		r, err = f.nextNonWhite(rs)
	}
	if err == nil {
		err = rs.UnreadRune()
	}
	if err != nil {
		if errors.Is(err, io.EOF) && 2 < len(args) && args[2] == nil {
			var result slip.Object
			if 3 < len(args) {
				result = args[3]
			}
			return result
		}
		slip.StreamPanic(s, depth, ss, "read failed. %s", err)
	}
	return slip.Character(r)
}

func (f *PeekChar) nextNonWhite(rs io.RuneScanner) (r rune, err error) {
	for {
		r, _, err = rs.ReadRune()
		if err != nil {
			break
		}
		switch r {
		case ' ', '\n', '\r', '\t', '\b', '\f':
			// keep going
		default:
			return
		}
	}
	return
}
