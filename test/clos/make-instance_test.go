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
	berry := slip.ReadEvalString(`
(defflavor blueberry ((size "medium")) () :initable-instance-variables)
(setq bb (make-instance 'blueberry :size 'small))
`, scope)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{"size":"small"}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	berry := slip.ReadEvalString(`
(defflavor blueberry () () (:default-init-plist (:x 3)))
(setq bb (make-instance 'blueberry :x 4))
`, scope)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceOtherKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	berry := slip.ReadEvalString(`
(defflavor blueberry () () (:default-init-plist (:allow-other-keys t)))
(setq bb (make-instance 'blueberry :x 4))
`, scope)
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceBadArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(make-instance)`, scope) })
}

func TestMakeInstanceNotFlavor(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(make-instance t)`, scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(make-instance 'not-a-flavor)`, scope) })
}

func TestMakeInstanceAbstract(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor blueberry ((size "medium")) () :abstract-flavor)
(make-instance blueberry)
`, scope)
	})
}

func TestMakeInstanceBadKeyword(t *testing.T) {
	defer undefFlavors("blueberry", "raspberry", "blackberry", "cherry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor blueberry () ())
(make-instance 'blueberry t nil)
`, scope)
	})
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor raspberry () ())
(make-instance raspberry bad nil)
`, scope)
	})
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor blackberry () ())
(make-instance blackberry :self nil)
`, scope)
	})
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor cherry () ())
(make-instance cherry :x nil)
`, scope)
	})
}

func TestMakeInstanceMissingKeyword(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`
(defflavor blueberry () () (:required-init-keywords :x))
(make-instance 'blueberry)
`, scope)
	})
}

func TestMakeInstanceNotFound(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`(make-instance 'nothing)`, scope)
	})
}

func TestMakeInstanceBuiltIn(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`(make-instance 'fixnum)`, scope)
	})
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`(make-instance (find-class 'fixnum))`, scope)
	})
}

func TestMakeInstanceCondition(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`(makunbound 'quux)`, scope)
	_ = slip.ReadEvalString(`(define-condition quux () ())`, scope).(*clos.ConditionClass)
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`(make-instance 'quux)`, scope)
	})
}
