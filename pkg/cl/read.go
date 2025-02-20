// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"errors"
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Read{Function: slip.Function{Name: "read", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "read",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "input-stream",
					Type: "input-stream",
					Text: "The stream to read from.",
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
			},
			Return: "object",
			Text: `__read__ from _input-stream_ and returns the value read. Note that unlike Common LISP
this function reads to the end of the input stream and is not suitable for repeated reads on the same stream.`,
			Examples: []string{
				`(read (make-string-input-stream "123 ")) => 123`,
			},
		}, &slip.CLPkg)
}

// Read represents the read function.
type Read struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Read) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 3)
	r := s.Get("*standard-input*").(io.Reader)
	eofp := true
	var eofv slip.Object

	if 0 < len(args) {
		if args[0] != nil {
			var ok bool
			if r, ok = args[0].(io.Reader); !ok {
				slip.PanicType("input-stream", args[0], "input-stream")
			}
		}
		if 1 < len(args) {
			eofp = args[1] != nil
			if 2 < len(args) {
				eofv = args[2]
			}
		}
	}
	return f.wrapRead(r, eofp, eofv)
}

func (f *Read) wrapRead(r io.Reader, eofp bool, eofv slip.Object) (result slip.Object) {
	defer func() {
		if rec := recover(); rec != nil {
			if eofp {
				panic(rec)
			}
			result = eofv
		}
	}()
	if seeker, ok := r.(io.Seeker); ok {
		start, err := seeker.Seek(0, io.SeekCurrent)
		if err != nil {
			panic(err)
		}
		code, pos := slip.ReadStream(r, true)
		if 0 < len(code) {
			if _, err = seeker.Seek(start+int64(pos), io.SeekStart); err != nil {
				panic(err)
			}
			return code[0]
		}
	} else {
		var (
			code slip.Code
			buf  []byte
			pos  int
			prev int
		)
		b := []byte{0}
		for {
			if n, err := r.Read(b); err != nil || n != 1 {
				if err != nil && !errors.Is(err, io.EOF) {
					panic(err)
				}
				break
			}
			buf = append(buf, b[0])
			code, pos = readOne(buf)
			if 0 < len(code) {
				if prev == pos {
					break
				}
				// Some types are only complete with a terminating
				// character. If one of those types has been read then break
				// out. Other like number ot symbols may or may not be
				// complete.
				switch code[0].(type) {
				case slip.List, slip.String, *slip.Vector, *slip.Array:
					return code[0]
				}
			}
			prev = pos
		}
		if 0 < len(code) {
			return code[0]
		}
	}
	if eofp {
		slip.PanicStream(r.(slip.Stream), "end of file or stream")
	}
	return eofv
}

func readOne(buf []byte) (code slip.Code, pos int) {
	defer func() {
		if rec := recover(); rec != nil {
			if _, ok := rec.(*slip.PartialPanic); !ok {
				panic(rec)
			}
		}
	}()
	return slip.ReadOne(buf)
}
