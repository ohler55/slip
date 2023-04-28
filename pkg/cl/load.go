// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Load{Function: slip.Function{Name: "load", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "load",
			Args: []*slip.DocArg{
				{
					Name: "filespec",
					Type: "stream|pathname",
					Text: `The source to read from.`,
				},
				{Name: "&key"},
				{
					Name: "verbose",
					Type: "boolean",
					Text: `If true (_t_) prints information on the file being loaded in the form
of a comment. The default is _*load-verbose*_.`,
				},
				{
					Name: "print",
					Type: "boolean",
					Text: `If true (_t_) prints the progress of the loading. The default is _*load-print*_.`,
				},
				{
					Name: "if-does-not-exist",
					Type: "boolean",
					Text: `TBD`,
				},
				{
					Name: "external-format",
					Type: "keyword",
					Text: `Ignored.`,
				},
			},
			Return: "list",
			Text:   `__load__ TBD.`,
			Examples: []string{
				`(load ???`,
			},
		}, &slip.CLPkg)
}

// Load represents the load function.
type Load struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Load) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 9)

	verbose := s.Get(slip.Symbol("*load-verbose*"))
	print := s.Get(slip.Symbol("*load-print*"))
	var (
		ifNotExist slip.Object = slip.True
		buf        []byte
		path       string
		err        error
	)
	for pos := 1; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":verbose":
			verbose = args[pos+1]
		case ":print":
			print = args[pos+1]
		case ":if-does-not-exist":
			ifNotExist = args[pos+1]
		case ":external-format":
			// ignore
		default:
			slip.PanicType("keyword", sym, ":verbose", ":print", ":if-does-not-exist", "external-format")
		}
	}
	defer func() {
		s.Set(slip.Symbol("*load-pathname*"), nil)
		s.Set(slip.Symbol("*load-truename*"), nil)
	}()
	switch ta := args[0].(type) {
	case slip.String:
		path = filepath.Join(slip.WorkingDir, string(ta))
		if buf, err = os.ReadFile(string(ta)); err != nil {
			if os.IsNotExist(err) && ifNotExist == nil {
				return nil
			}
			panic(fmt.Sprintf("loading %s: %s", path, err))
		}
	case io.Reader:
		path = slip.ObjectString(args[0])
		if buf, err = io.ReadAll(ta); err != nil {
			panic(fmt.Sprintf("loading %s: %s", path, err))
		}
	default:
		slip.PanicType("filespec", ta, "stream", "string")
	}
	s.Set(slip.Symbol("*load-pathname*"), slip.String(path))
	s.Set(slip.Symbol("*load-truename*"), slip.String(path))
	var w io.Writer
	if verbose != nil || print != nil {
		if w, _ = s.Get("*standard-output*").(io.Writer); w != nil {
			_, _ = fmt.Fprintf(w, ";; Loading contents of %s\n", path)
			defer func() { _, _ = fmt.Fprintf(w, ";; Finished loading %s\n", path) }()
		}
	}
	code := slip.Read(buf)
	code.Compile()
	if print == nil {
		code.Eval(s, nil)
	} else {
		code.Eval(s, w)
	}
	return slip.True
}
