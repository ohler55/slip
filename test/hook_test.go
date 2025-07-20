// Copyright (c) 2024, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestSetHook(t *testing.T) {
	var log []byte
	slip.AddSetHook("hook-set", func(p *slip.Package, key string) { log = fmt.Appendf(log, "set %s\n", key) })
	slip.AddUnsetHook("hook-unset", func(p *slip.Package, key string) { log = fmt.Appendf(log, "unset %s\n", key) })
	slip.AddClassHook("hook-class", func(p *slip.Package, key string) { log = fmt.Appendf(log, "class %s\n", key) })

	slip.CurrentPackage.Set("set-hook-test", slip.True)
	slip.CurrentPackage.Remove("set-hook-test")

	tt.Equal(t, "set set-hook-test\nunset set-hook-test\n", string(log))

	slip.RemoveSetHook("hook-set")
	slip.RemoveUnsetHook("hook-unset")
	slip.RemoveClassHook("hook-class")

	slip.CurrentPackage.Set("set-hook-test", slip.True)
	slip.CurrentPackage.Remove("set-hook-test")

	// Should not have changed.
	tt.Equal(t, "set set-hook-test\nunset set-hook-test\n", string(log))
}

func TestDefunHook(t *testing.T) {
	var log []byte
	slip.AddDefunHook("hook-fun", func(p *slip.Package, key string) { log = fmt.Appendf(log, "fun %s\n", key) })

	scope := slip.NewScope()
	_ = slip.ReadString(`(defun hook-func () nil)`, scope).Eval(scope, nil)

	slip.RemoveDefunHook("hook-fun")
	_ = slip.ReadString(`(defun hook-func-2 () nil)`, scope).Eval(scope, nil)

	tt.Equal(t, "fun hook-func\n", string(log))
}
