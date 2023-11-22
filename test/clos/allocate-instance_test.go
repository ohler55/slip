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
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) () :initable-instance-variables)
(setq bb (allocate-instance 'blueberry :size 'small))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope, nil)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{"size":"small"}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestAllocateInstanceKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry () () (:default-init-plist (:x 3)))
(setq bb (allocate-instance 'blueberry :x 4))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope, nil)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestAllocateInstanceOtherKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry () () (:default-init-plist (:allow-other-keys t)))
(setq bb (allocate-instance 'blueberry :x 4))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope, nil)
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestAllocateInstanceBadArgCount(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString(`(allocate-instance)`).Eval(slip.NewScope(), nil) })
}

func TestAllocateInstanceNotFlavor(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString(`(allocate-instance t)`).Eval(slip.NewScope(), nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(allocate-instance 'not-a-flavor)`).Eval(slip.NewScope(), nil) })
}

func TestAllocateInstanceAbstract(t *testing.T) {
	defer undefFlavors("blueberry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry ((size "medium")) () :abstract-flavor)
(allocate-instance blueberry)
`).Eval(slip.NewScope(), nil)
	})
}

func TestAllocateInstanceBadKeyword(t *testing.T) {
	defer undefFlavors("blueberry", "raspberry", "blackberry", "cherry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry () ())
(allocate-instance 'blueberry t nil)
`).Eval(slip.NewScope(), nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor raspberry () ())
(allocate-instance raspberry bad nil)
`).Eval(slip.NewScope(), nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blackberry () ())
(allocate-instance blackberry :self nil)
`).Eval(slip.NewScope(), nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor cherry () ())
(allocate-instance cherry :x nil)
`).Eval(slip.NewScope(), nil)
	})
}

func TestAllocateInstanceMissingKeyword(t *testing.T) {
	defer undefFlavors("blueberry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry () () (:required-init-keywords :x))
(allocate-instance 'blueberry)
`).Eval(slip.NewScope(), nil)
	})
}
