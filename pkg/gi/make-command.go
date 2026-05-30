// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"os/exec"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defMakeCommand() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeCommand{Function: slip.Function{Name: "make-command", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-command",
			Args: []*slip.DocArg{
				{
					Name: "path",
					Type: "string",
					Text: "The path to the command to run.",
				},
				{Name: "&rest"},
				{
					Name: "args",
					Type: "string|list",
					Text: `The command line argument to the command as a list of strings or
as multiple string designator arguments. Any non-string arguments will be converted to strings.`,
				},
			},
			Return: "command",
			Text:   `__make-command__ make the command with a _path_ and _args_ provided.`,
			Examples: []string{
				`(make-command  "sleep" 1) => #<command 12345>`,
			},
		}, &Pkg)
}

// MakeCommand represents the make-command function.
type MakeCommand struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeCommand) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	path := slip.MustBeString(args[0], "path")
	cargs := make([]string, len(args)-1)
	for i, a := range args[1:] {
		if ss, ok := a.(slip.String); ok {
			cargs[i] = string(ss)
		} else {
			cargs[i] = slip.ObjectString(a)
		}
	}
	command := exec.Command(path, cargs...)

	inst := commandFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = command

	return inst
}
