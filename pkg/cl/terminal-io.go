// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.CLPkg.Locked = false
	slip.CLPkg.Set("*terminal-io*", NewTwoWayStream(
		slip.NewInputStream(os.Stdin),
		&slip.OutputStream{Writer: os.Stdout},
	))
	slip.CLPkg.Locked = true
}
