// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"errors"
	"os"
	"path/filepath"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UseStash{Function: slip.Function{Name: "use-stash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "use-stash",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string",
					Text: "Name of stash or path to the stash file.",
				},
			},
			Return: "nil",
			Text: `__use-stash__ loads and initializes the stash with the specified _name_. If
_name_ includes a '/' character or ends in '.lisp' then the _name_ is assumed
to be a path to a stash file which is a regular LISP file. Otherwise the
_name_ is assumed to be the name of a stash file that can be found in one of
the _*stash-load-path*_ directories with a filename of the _name_ with a
'.lisp' suffix. If no existing file is found then one is created at the first
entry in _*stash-load-path*_ or in the current directory if the
_*stash-load-path*_ is empty.


A stash is a file backed set of forms that is grown by adding to the stash
using either the _stash-form_ function or pressing the _stash-form_ key
(M-s). The stash can be shown with the _show-stash_ and individual forms in
the stash can be retrieved with the _nth-stash_ function of the _nth-stash_
key (M-S). The _clear-stash_ can be used to clear the stash or a subset of the
forms in the stash.


Since the stash is stored on disk as a LISP file it can be edited directly the
_edit-stash_ which will open the file in $EDITOR or
_*repl-external-editor*_. Note that editing the file without calling
_edit-stash_ may result in an inconsistent cache if slip is running and a new
stash entry is made after or during editing.

`,
		}, &Pkg)
}

// UseStash represents the use-stash function.
type UseStash struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *UseStash) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	name := slip.MustBeString(args[0], "name")
	if len(name) == 0 {
		slip.NewPanic("a stash filename can not be empty")
	}
	home, _ := os.UserHomeDir()
	if strings.Contains(name, "/") || strings.HasSuffix(strings.ToLower(name), ".lisp") {
		name = strings.ReplaceAll(name, "~", home)
		if _, err := os.Stat(name); err != nil && errors.Is(err, os.ErrNotExist) {
			if err = os.WriteFile(name, []byte{}, 0666); err != nil {
				slip.NewPanic("failed to open or create a stash file at %s", name)
			}
		}
		TheStash.LoadExpanded(name)
		return nil
	}
	name += ".lisp"
	loadPaths, _ := s.Get("*stash-load-path*").(slip.List)
	if len(loadPaths) == 0 {
		loadPaths = append(loadPaths, slip.String("."))
	}
	for _, spath := range loadPaths {
		if path, ok := spath.(slip.String); ok {
			fp := strings.ReplaceAll(filepath.Join(string(path), name), "~", home)
			if fi, err := os.Stat(fp); err != nil || fi.IsDir() {
				continue
			}
			TheStash.LoadExpanded(fp)
			return nil
		}
	}
	if path, ok := loadPaths[0].(slip.String); ok {
		fp := strings.ReplaceAll(filepath.Join(string(path), name), "~", home)
		if err := os.WriteFile(fp, []byte{}, 0666); err != nil {
			slip.NewPanic("failed to open or create a stash file at %s", fp)
		}
		TheStash.LoadExpanded(fp)
	}
	return nil
}
