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
				{
					Name: "permission",
					Type: "fixnum",
					Text: "The file permissions when creating a file. This is not a common LISP standard.",
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
    __:new-version__ not supported.
    __:rename-and-delete__ not supported.
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
				`(open "test.json") => #<FILE-STREAM test.json {5}>`,
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

func (f *Open) openFile(args slip.List) slip.Object {
	slip.ArgCountCheck(f, args, 1, 9)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("filepath", args[0], "string")
	}
	var (
		flags    int
		perm     fs.FileMode = 0666
		nilError bool
		probe    bool
		rename   bool
	)
	for pos := 1; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			panic(fmt.Sprintf("%s missing an argument", sym))
		}
		val := args[pos+1]
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
				probe = true
			default:
				slip.PanicType(string(sym), val, ":input", ":output", ":io", ":probe")
			}
		case ":if-exists":
			switch val {
			case nil:
				nilError = true
				flags |= os.O_EXCL
			case slip.Symbol(":error"):
				flags |= os.O_EXCL
			case slip.Symbol(":rename"):
				rename = true
				flags |= os.O_CREATE
			case slip.Symbol(":overwrite"):
				// normal with no other flags
			case slip.Symbol(":append"):
				flags |= os.O_APPEND
			case slip.Symbol(":supersede"):
				flags |= os.O_TRUNC | os.O_APPEND
			case slip.Symbol(":new-version"), slip.Symbol(":rename-and-delete"):
				// not supported
			default:
				slip.PanicType(string(sym), val,
					":error", ":new-version", ":rename", ":overwrite", ":append", ":supersede", "nil")
			}
		case ":if-does-not-exist":
			switch val {
			case nil:
				nilError = true
			case slip.Symbol(":error"):
				// default is to report errors
			case slip.Symbol(":create"):
				flags |= os.O_CREATE
			default:
				slip.PanicType(string(sym), val, ":error", ":create", "nil")
			}
		case ":permissions":
			var num slip.Fixnum
			if num, ok = val.(slip.Fixnum); ok {
				perm = fs.FileMode(num)
			} else {
				slip.PanicType(string(sym), val, "fixnum")
			}
		default:
			slip.PanicType("keyword", sym, ":direction", ":if-exists", ":if-does-not-exist")
		}
	}
	if rename {
		if _, err := os.Stat(string(path)); err == nil {
			if err = os.Rename(string(path), string(path)+".bak"); err != nil {
				panic(err)
			}
		}
	}
	file, err := os.OpenFile(string(path), flags, perm)
	if err != nil {
		if !nilError {
			panic(err)
		}
		return nil
	}
	if probe {
		_ = file.Close()
	}
	return (*slip.FileStream)(file)
}
