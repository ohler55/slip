// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestMakeInstanceSimple(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) () :initable-instance-variables)
(setq bb (make-instance 'blueberry :size 'small))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope, nil)

	tt.Equal(t, "/{flavor: blueberry id: \"[0-9a-f]+\" vars: {size: small}}/", pretty.SEN(berry))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry () () (:default-init-plist (:x 3)))
(setq bb (make-instance 'blueberry :x 4))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope, nil)

	tt.Equal(t, "/{flavor: blueberry id: \"[0-9a-f]+\" vars: {}}/", pretty.SEN(berry))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceOtherKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry () () (:default-init-plist (:allow-other-keys t)))
(setq bb (make-instance 'blueberry :x 4))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope, nil)
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceBadArgCount(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance)`).Eval(slip.NewScope(), nil) })
}

func TestMakeInstanceNotFlavor(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance t)`).Eval(slip.NewScope(), nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'not-a-flavor)`).Eval(slip.NewScope(), nil) })
}

func TestMakeInstanceAbstract(t *testing.T) {
	defer undefFlavors("blueberry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry ((size "medium")) () :abstract-flavor)
(make-instance blueberry)
`).Eval(slip.NewScope(), nil)
	})
}

func TestMakeInstanceBadKeyword(t *testing.T) {
	defer undefFlavors("blueberry", "raspberry", "blackberry", "cherry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry () ())
(make-instance 'blueberry t nil)
`).Eval(slip.NewScope(), nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor raspberry () ())
(make-instance raspberry bad nil)
`).Eval(slip.NewScope(), nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blackberry () ())
(make-instance blackberry :self nil)
`).Eval(slip.NewScope(), nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor cherry () ())
(make-instance cherry :x nil)
`).Eval(slip.NewScope(), nil)
	})
}

func TestMakeInstanceMissingKeyword(t *testing.T) {
	defer undefFlavors("blueberry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry () () (:required-init-keywords :x))
(make-instance 'blueberry)
`).Eval(slip.NewScope(), nil)
	})
}
