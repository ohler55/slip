// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"fmt"
	"io"

	"github.com/ohler55/slip"
)

var (
	// Pkg is the watch package.
	Pkg = slip.Package{
		Name:      "watch",
		Nicknames: []string{},
		Doc: `

The _watch_ package facilitates inspection and evaluation on a remote SLIP
process. A _watch-server_ is set up in the server process and one or more
clients can then connect with the server and make a request to evaluate an
expression, watch glbal variables, or periodically evaluate and expression and
return the results.

__Protocol__


__Use__


__Examples__



_Clients_


_Methods_

`,
		PreSet: slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(map[string]*slip.VarVal{})
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*watch*", &Pkg)
}

func displayError(format string, args ...any) {
	eo := slip.NewScope().Get("*error-output*").(io.Writer)
	fmt.Fprintf(eo, "\n*-*-* %s\n", fmt.Sprintf(format, args...))
}
