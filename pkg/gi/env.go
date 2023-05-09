// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"os"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Env{Function: slip.Function{Name: "env", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "env",
			Args:   []*slip.DocArg{},
			Return: "list",
			Text:   `__env__ returns the environment variables as an association list.`,
			Examples: []string{
				`(env) => (("TEST_NAME" . "something"))`,
			},
		}, &Pkg)
}

// Env represents the env function.
type Env struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Env) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	var alist slip.List
	for _, ev := range os.Environ() {
		parts := strings.Split(ev, "=")
		alist = append(alist, slip.List{slip.String(parts[0]), slip.Tail{Value: slip.String(parts[1])}})
	}
	return alist
}
