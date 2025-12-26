# SLIP Plugins

SLIP can be extended with plugins using the **Go** plugin
package. However, there are some guidelines that should be followed
not only for SLIP plugins but for all **Go** plugins.

A plugin is a **Go** package that can be loaded at runtime. In SLIP
that is accomplished with the `require` function. To define a module
with functions, variables, and constants a plugin package is defined
and a `.so` file built. Best practices place the `.so` files in a
common directory once the `.so` file is built.

It is important to make sure the plugin is built with the **exact**
version of SLIP that will be used to `require` the module. **Go** is
extremely sensitive to any deviation in versions. Another area to
watch for is requiring plugins that depend on different version of
some other package as defined in the go.mod files for each plugin. The
only indication of a mismatch is a failure with no description of the
cause.

A trivial example of a plugin for SLIP is:

```go
package main

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Plug{Function: slip.Function{Name: "plug", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "plug",
			Args:   []*slip.DocArg{},
			Return: "string",
		}, &Pkg)
}

// Plug represents the plug function.
type Plug struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Plug) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return slip.String("Plugged it")
}
```
