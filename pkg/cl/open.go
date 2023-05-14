// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io/fs"
	"os"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Open{Function: slip.Function{Name: "open", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "open",
			Args: []*slip.DocArg{
				{
					Name: "filepath",
					Type: "string",
					Text: "The filepath to open.",
				},
				{Name: "&key"},
				{
					Name: "direction",
					Type: "symbol",
					Text: "The direction of the file stream.",
				},
				{
					Name: "if-exists",
					Type: "symbol",
					Text: "The action to take if the file already exists.",
				},
				{
					Name: "if-does-not-exist",
					Type: "symbol",
					Text: "The action to take if the file does not exist.",
				},
			},
			Return: "file-stream",
			Text: `__open__ returns a _file-stream_ opened according to the keyword arguments.
  __:direction__ can be one of:
    __:input__ opens the file for input.
    __:output__ opens the file for output.
    __:io__ opens the file for input and output.
    __:probe__ opens the file then closes it.
  __:if-exists__ describes the action to take if the file exists.
    __:error__ an error (panic) is raised.
    __:new-version__ creates a new file with a larger version.
    __:rename__ renames the existing file before creating the file.
    __:overwrite__ modifies the existing file.
    __:append__ writes to the file will be appended.
    __:supersede__ replaces the old file.
    __nil__ a new file is not created and nil is returned.
  __:if-does-not-exist__ specifies the action to take if the file does not exist.
    __:error__ an error (panic) is raised.
    __:create__ creates a new file.
    __nil__ no file is created and nil is returned.

`,
			Examples: []string{
				`(open "../*.lisp") => ("/top/one/x.lisp" "/top/one/y.lisp")`,
			},
		}, &slip.CLPkg)
}

// Open represents the open function.
type Open struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Open) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return f.openFile(args)
}

func (f *Open) openFile(args slip.List) *slip.FileStream {
	slip.ArgCountCheck(f, args, 1, 7)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("filepath", args[0], "string")
	}
	var (
		exists   slip.Object
		notExist slip.Object
		flags    int
		perm     fs.FileMode = 0666
		nilError bool
		// TBD set flag options
		// TBD add perm to keyword options, default to 0666
	)
	for pos := 1; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			panic(fmt.Sprintf("%s missing an argument", sym))
		}
		var val slip.Object
		switch ta := args[pos+1].(type) {
		case nil:
			// leave as nil
		case slip.Symbol:
			val = slip.Symbol(strings.ToLower(string(ta)))
		default:
			slip.PanicType(string(sym), ta, "symbol", "nil")
		}
		switch strings.ToLower(string(sym)) {
		case ":direction":
			switch val {
			case slip.Symbol(":input"):
				flags |= os.O_RDONLY
			case slip.Symbol(":output"):
				flags |= os.O_WRONLY
			case slip.Symbol(":io"):
				flags |= os.O_RDWR
			case slip.Symbol(":probe"):
				// TBD probe = true
			default:
				slip.PanicType(string(sym), val, ":input", ":output", ":io", ":probe")
			}
		case ":if-exists":
			exists = val
			switch val {
			case nil:
				nilError = true
			case slip.Symbol(":error"):
				flags |= os.O_EXCL
			case slip.Symbol(":new-version"):
				// TBD
			case slip.Symbol(":rename"):
				// TBD
			case slip.Symbol(":overwrite"):
				flags |= os.O_APPEND
				// TBD set pos to 0
			case slip.Symbol(":append"):
				flags |= os.O_APPEND
			case slip.Symbol(":supersede"):
				flags |= os.O_TRUNC | os.O_APPEND
			default:
				slip.PanicType(string(sym), val, ":error", ":new-version", ":rename", ":overwrite", ":append", ":supersede", "nil")
			}
		case ":if-does-not-exists":
			notExist = val
			switch val {
			case nil:
				nilError = true
			case slip.Symbol(":error"):
			case slip.Symbol(":create"):
				flags |= os.O_CREATE
			default:
				slip.PanicType(string(sym), val, ":error", ":create", "nil")
			}
		default:
			slip.PanicType("keyword", sym, ":direction", ":if-exists", ":if-does-not-exist")
		}
	}
	fmt.Printf("*** path: %q exists: %s not-exist: %s flags: %x\n", path, exists, notExist, flags)
	file, err := os.OpenFile(string(path), flags, perm)
	if err != nil {
		if !nilError {
			panic(err)
		}
		return nil
	}
	return (*slip.FileStream)(file)
}
