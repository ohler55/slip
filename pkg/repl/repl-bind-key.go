// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReplBindKey{Function: slip.Function{Name: "repl-bind-key", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "repl-bind-key",
			Args: []*slip.DocArg{
				{
					Name: "key",
					Type: "string",
					Text: `The key name such as "C-l", "M-f", or "M-[1;5C".`,
				},
				{
					Name: "action",
					Type: "symbol",
					Text: "The action to bind to the key or _nil_ to disable the key.",
				},
			},
			Return: "symbol",
			Text: `__repl-bind-key__ binds _key_ to _action_ in the REPL editor and saves the
binding in the configuration file. The actions available are listed by _repl-key-actions_.
Key names follow Emacs conventions where __M-__ is the escape prefix, __C-__ is a
control key, and __DEL__, __TAB__, __RET__, __ESC__, and __SPC__ are named keys. Other
characters are used as is. Terminal keys are named in angle brackets such as "<home>",
"C-<right>", or "<f5>" and a name binds every sequence for the key. A key that is a
prefix for other keys can not be bound.`,
			Examples: []string{
				`(repl-bind-key "C-l" 'clear-form) => clear-form`,
				`(repl-bind-key "C-<right>" 'forward-word) => forward-word`,
			},
		}, &Pkg)
}

// ReplBindKey represents the repl-bind-key function.
type ReplBindKey struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ReplBindKey) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	kb := bindingKey(s, depth, args[0])
	kb.act = findAction(s, depth, args[1])
	kbs := append([]keyBinding{}, userBinds.Load().list...)
	setKeyBindings(addKeyBinding(kbs, kb))
	setHook(&Pkg, keyBindingsName)

	return actionSymbol(kb.act)
}
