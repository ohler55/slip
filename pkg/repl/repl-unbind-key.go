// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReplUnbindKey{Function: slip.Function{Name: "repl-unbind-key", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "repl-unbind-key",
			Args: []*slip.DocArg{
				{
					Name: "key",
					Type: "string",
					Text: `The key name such as "C-l", "M-f", or "M-[1;5C".`,
				},
			},
			Return: "boolean",
			Text: `__repl-unbind-key__ removes the user binding for _key_ which restores the
default binding and saves the change in the configuration file. Returns _t_ if a
binding was removed and _nil_ otherwise.`,
			Examples: []string{
				`(repl-unbind-key "C-l") => t`,
			},
		}, &Pkg)
}

// ReplUnbindKey represents the repl-unbind-key function.
type ReplUnbindKey struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ReplUnbindKey) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	key, _ := parseKeyArg(s, depth, args[0])
	list := userBinds.Load().list
	for i, kb := range list {
		if kb.key == key {
			kbs := append(append([]keyBinding{}, list[:i]...), list[i+1:]...)
			setKeyBindings(kbs)
			setHook(&Pkg, keyBindingsName)
			return slip.True
		}
	}
	return nil
}
