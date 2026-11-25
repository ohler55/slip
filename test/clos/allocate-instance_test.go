// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg"
	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg"
)

func TestAllocateInstanceSimple(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	berry := slip.ReadEvalString(`
(defflavor blueberry ((size "medium")) () :initable-instance-variables)
(setq bb (allocate-instance 'blueberry :size 'small))
`, scope)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{"size":"small"}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestAllocateInstanceKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	berry := slip.ReadEvalString(`
(defflavor blueberry () () (:default-init-plist (:x 3)))
(setq bb (allocate-instance 'blueberry :x 4))
`, scope)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestAllocateInstanceOtherKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	berry := slip.ReadEvalString(`
(defflavor blueberry () () (:default-init-plist (:allow-other-keys t)))
(setq bb (allocate-instance 'blueberry :x 4))
`, scope)
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestAllocateInstanceBadArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(allocate-instance)`, scope) })
}

func TestAllocateInstanceNotFlavor(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(allocate-instance t)`, scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(allocate-instance 'not-a-flavor)`, scope) })
}

func TestAllocateInstanceAbstract(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor blueberry ((size "medium")) () :abstract-flavor)
(allocate-instance blueberry)
`, scope)
	})
}

func TestAllocateInstanceBadKeyword(t *testing.T) {
	defer undefFlavors("blueberry", "raspberry", "blackberry", "cherry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor blueberry () ())
(allocate-instance 'blueberry t nil)
`, scope)
	})
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor raspberry () ())
(allocate-instance raspberry bad nil)
`, scope)
	})
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor blackberry () ())
(allocate-instance blackberry :self nil)
`, scope)
	})
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor cherry () ())
(allocate-instance cherry :x nil)
`, scope)
	})
}

func TestAllocateInstanceMissingKeyword(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor blueberry () () (:required-init-keywords :x))
(allocate-instance 'blueberry)
`, scope)
	})
}
