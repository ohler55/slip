// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos_test

import (
	"fmt"

	"github.com/ohler55/slip"
)

func undefFlavors(fns ...string) {
	for _, fn := range fns {
		undefFlavor(fn)
	}
}

func undefFlavor(fn string) {
	defer func() { _ = recover() }()
	scope := slip.NewScope()
	slip.ReadString(fmt.Sprintf("(undefflavor '%s)", fn), scope).Eval(scope, nil)
}
