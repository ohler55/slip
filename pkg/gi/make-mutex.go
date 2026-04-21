// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"sync"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeMutex{Function: slip.Function{Name: "make-mutex", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "make-mutex",
			Args:   []*slip.DocArg{},
			Return: "mutex",
			Text:   `__make-mutex__ `,
			Examples: []string{
				`(make-mutex) => #<mutex 12345>`,
			},
		}, &Pkg)
}

// MakeMutex represents the make-mutex function.
type MakeMutex struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeMutex) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 0, 0)
	var m sync.Mutex

	return (*Mutex)(&m)
}
