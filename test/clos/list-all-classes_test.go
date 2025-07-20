// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestListAllClassesNoKey(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	slip.CurrentPackage.Remove("buux")
	(&sliptest.Function{
		Source: `(progn (defclass buux () ()) (defclass quux (buux) ()) (list-all-classes))`,
		Validate: func(t *testing.T, v slip.Object) {
			list, ok := v.(slip.List)
			tt.Equal(t, true, ok)
			tt.Equal(t, true, classInList("simple-type-error", list)) // condition
			tt.Equal(t, true, classInList("bag-flavor", list))        // flavor
			tt.Equal(t, true, classInList("quux", list))              // standard
			tt.Equal(t, true, classInList("fixnum", list))            // built-in
		},
	}).Test(t)
}

func TestListAllClassesMeta(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	slip.CurrentPackage.Remove("buux")
	(&sliptest.Function{
		Source: `(progn (defclass buux () ()) (defclass quux (buux) ()) (list-all-classes :metaclass 'standard-class))`,
		Validate: func(t *testing.T, v slip.Object) {
			list, ok := v.(slip.List)
			tt.Equal(t, true, ok)
			tt.Equal(t, false, classInList("simple-type-error", list)) // condition
			tt.Equal(t, false, classInList("bag-flavor", list))        // flavor
			tt.Equal(t, true, classInList("quux", list))               // standard
			tt.Equal(t, false, classInList("fixnum", list))            // built-in
		},
	}).Test(t)
}

func TestListAllClassesPackage(t *testing.T) {
	slip.CurrentPackage.Remove("quux")
	slip.CurrentPackage.Remove("buux")
	(&sliptest.Function{
		Source: `(progn (defclass buux () ()) (defclass quux (buux) ()) (list-all-classes :package 'net))`,
		Validate: func(t *testing.T, v slip.Object) {
			list, ok := v.(slip.List)
			tt.Equal(t, true, ok)
			tt.Equal(t, false, classInList("simple-type-error", list)) // condition
			tt.Equal(t, false, classInList("bag-flavor", list))        // flavor
			tt.Equal(t, false, classInList("quux", list))              // standard
			tt.Equal(t, false, classInList("fixnum", list))            // built-in
			tt.Equal(t, true, classInList("http-client-flavor", list)) // in net package
		},
	}).Test(t)
	(&sliptest.Function{
		Source: `(list-all-classes :package (find-package 'net))`,
		Validate: func(t *testing.T, v slip.Object) {
			list, ok := v.(slip.List)
			tt.Equal(t, true, ok)
			tt.Equal(t, true, classInList("http-client-flavor", list)) // in net package
		},
	}).Test(t)
	(&sliptest.Function{
		Source: `(list-all-classes :package "net")`,
		Validate: func(t *testing.T, v slip.Object) {
			list, ok := v.(slip.List)
			tt.Equal(t, true, ok)
			tt.Equal(t, true, classInList("http-client-flavor", list)) // in net package
		},
	}).Test(t)
}

func TestListAllClassesNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(list-all-classes :package 'not-a-package)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(list-all-classes :package "not-a-package")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(list-all-classes :package t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func classInList(name string, list slip.List) bool {
	for _, v := range list {
		if c, ok := v.(slip.Class); ok && name == c.Name() {
			return true
		}
	}
	return false
}
