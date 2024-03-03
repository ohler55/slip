// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"os"
	"os/exec"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := EditStash{Function: slip.Function{Name: "edit-stash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "edit-stash",
			Args:   []*slip.DocArg{},
			Return: "nil",
			Text:   `__edit-stash__ opens the stash in an editor.`,
		}, &Pkg)
}

// EditStash represents the edit-stash function.
type EditStash struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *EditStash) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)

	var xargs []string
	xed := externalEditor
	if len(xed) == 0 {
		xed = os.Getenv("EDITOR")
		if len(xed) == 0 {
			slip.NewPanic("no editor defined")
		}
		parts := strings.Split(xed, " ")
		xed = parts[0]
		if len(editorFlags) == 0 {
			xargs = append(xargs, parts[1:]...)
		}
	}
	for _, sflag := range editorFlags {
		if flag, _ := sflag.(slip.String); 0 < len(flag) {
			xargs = append(xargs, string(flag))
		}
	}
	// TBD edit in place

	xargs = append(xargs, TheStash.filename)
	cmd := exec.Command(xed, xargs...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		slip.NewPanic("%s: %s\n", err, stderr.String())
	}
	TheStash.LoadExpanded(TheStash.filename)

	return nil
}
