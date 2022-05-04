// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"testing"

	"github.com/ohler55/slip"
)

func TestDefunBasic(t *testing.T) {
	code := slip.ReadString(`
(defun funny (x)
  "Documentation here"
  7)
(funny 7)`)
	scope := slip.NewScope()
	fmt.Printf("*** result: %s\n", code.Eval(scope))
}
