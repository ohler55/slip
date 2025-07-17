// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg"
	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
)

func TestMakeInstanceSimple(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) () :initable-instance-variables)
(setq bb (make-instance 'blueberry :size 'small))
`, scope)
	berry := code.Eval(scope, nil)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{"size":"small"}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor blueberry () () (:default-init-plist (:x 3)))
(setq bb (make-instance 'blueberry :x 4))
`, scope)
	berry := code.Eval(scope, nil)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceOtherKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor blueberry () () (:default-init-plist (:allow-other-keys t)))
(setq bb (make-instance 'blueberry :x 4))
`, scope)
	berry := code.Eval(scope, nil)
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceBadArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance)`, scope).Eval(scope, nil) })
}

func TestMakeInstanceNotFlavor(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance t)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'not-a-flavor)`, scope).Eval(scope, nil) })
}

func TestMakeInstanceAbstract(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry ((size "medium")) () :abstract-flavor)
(make-instance blueberry)
`, scope).Eval(scope, nil)
	})
}

func TestMakeInstanceBadKeyword(t *testing.T) {
	defer undefFlavors("blueberry", "raspberry", "blackberry", "cherry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry () ())
(make-instance 'blueberry t nil)
`, scope).Eval(scope, nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor raspberry () ())
(make-instance raspberry bad nil)
`, scope).Eval(scope, nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blackberry () ())
(make-instance blackberry :self nil)
`, scope).Eval(scope, nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor cherry () ())
(make-instance cherry :x nil)
`, scope).Eval(scope, nil)
	})
}

func TestMakeInstanceMissingKeyword(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry () () (:required-init-keywords :x))
(make-instance 'blueberry)
`, scope).Eval(scope, nil)
	})
}

func TestMakeInstanceNotFound(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance 'nothing)`, scope).Eval(scope, nil)
	})
}

func TestMakeInstanceBuiltIn(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance 'fixnum)`, scope).Eval(scope, nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance (find-class 'fixnum))`, scope).Eval(scope, nil)
	})
}

func TestMakeInstanceCondition(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(makunbound 'quux)`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(define-condition quux () ())`, scope).Eval(scope, nil).(*clos.ConditionClass)
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance 'quux)`, scope).Eval(scope, nil)
	})
}
