// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"io/fs"
	"os"
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FileInfo{Function: slip.Function{Name: "file-info", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "file-info",
			Args: []*slip.DocArg{
				{
					Name: "filepath",
					Type: "string",
					Text: "The file to get the info of.",
				},
			},
			Return: "property-list|nil",
			Text: `__file-info__ returns the file at _filepath__ information as an property list
or _nil_ if the info can not be determined.`,
			Examples: []string{
				`(file-info "three.lisp")`,
				`(:name "quux.lisp"`,
				` :path "/Users/quux/quux.lisp"`,
				` :size 4321`,
				` :perm "-rw-r--r--"`,
				` :type :regular`,
				` :mod-time @2025-01-18T22:32:32.916986193Z`,
				` :is-dir nil)`,
			},
		}, &Pkg)
}

// FileInfo represents the file-info function.
type FileInfo struct {
	slip.Function
}

var modeTypeMap = map[fs.FileMode]slip.Object{
	fs.ModeSymlink:    slip.Symbol(":symlink"),
	fs.ModeDir:        slip.Symbol(":directory"),
	fs.ModeNamedPipe:  slip.Symbol(":pipe"),
	fs.ModeSocket:     slip.Symbol(":socket"),
	fs.ModeCharDevice: slip.Symbol(":character-device"),
	fs.ModeSticky:     slip.Symbol(":sticky"),
}

// Call the function with the arguments provided.
func (f *FileInfo) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("filepath", args[0], "string")
	}
	if abs, err := filepath.Abs(string(path)); err == nil {
		var file *os.File
		if file, err = os.Open(abs); err == nil {
			var fi fs.FileInfo
			if fi, err = file.Stat(); err == nil {
				var (
					dir  slip.Object
					kind slip.Object
				)
				mode := fi.Mode()
				if fi.IsDir() {
					dir = slip.True
				}
				if mode.IsRegular() {
					kind = slip.Symbol(":regular")
				} else {
					kind = modeTypeMap[mode.Type()]
				}
				return slip.List{
					slip.Symbol(":name"), slip.String(fi.Name()),
					slip.Symbol(":path"), slip.String(abs),
					slip.Symbol(":size"), slip.Fixnum(fi.Size()),
					slip.Symbol(":perm"), slip.String(fi.Mode().String()),
					slip.Symbol(":type"), kind,
					slip.Symbol(":mod-time"), slip.Time(fi.ModTime().UTC()),
					slip.Symbol(":is-dir"), dir,
				}
			}
		}
	}
	return nil
}
