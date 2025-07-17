// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestDefSystem(t *testing.T) {
	scope := slip.NewScope()
	tf := sliptest.Function{
		Source: `(defsystem "quux"
                            :author "pete"
                            :maintainer "pete@ohler.com"
                            :license "MIT"
                            :version "v0.1.0"
                            :homepage "https://github.com/ohler55/slip"
                            :bug-tracker "https://github.com/ohler55/slip/issues"
                            :source-control "https://github.com/ohler55/slip"
                            :description "Just a sample."
                            :cache "testout"
                            :components '("testdata/comp")
                            :depends-on '((quux :file "testdata" :files ("sys-test" "sub")))
                            :in-order-to '((:sample (+ (sys-test) six)) (:just-eval 3)))`,
		Expect: "/#<system [0-9a-f]+>/",
	}
	tf.Test(t)
	sys, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)

	tt.Equal(t, `"quux"`, slip.ObjectString(sys.Receive(scope, ":name", nil, 0)))
	tt.Equal(t, `"pete"`, slip.ObjectString(sys.Receive(scope, ":author", nil, 0)))
	tt.Equal(t, `"pete@ohler.com"`, slip.ObjectString(sys.Receive(scope, ":maintainer", nil, 0)))
	tt.Equal(t, `"MIT"`, slip.ObjectString(sys.Receive(scope, ":license", nil, 0)))
	tt.Equal(t, `"v0.1.0"`, slip.ObjectString(sys.Receive(scope, ":version", nil, 0)))
	tt.Equal(t, `"https://github.com/ohler55/slip"`, slip.ObjectString(sys.Receive(scope, ":homepage", nil, 0)))
	tt.Equal(t, `"https://github.com/ohler55/slip/issues"`,
		slip.ObjectString(sys.Receive(scope, ":bug-tracker", nil, 0)))
	tt.Equal(t, `"https://github.com/ohler55/slip"`, slip.ObjectString(sys.Receive(scope, ":source-control", nil, 0)))
	tt.Equal(t, `"Just a sample."`, slip.ObjectString(sys.Receive(scope, ":description", nil, 0)))
	tt.Equal(t, `"testout"`, slip.ObjectString(sys.Receive(scope, ":cache", nil, 0)))
	tt.Equal(t, `("testdata/comp")`, slip.ObjectString(sys.Receive(scope, ":components", nil, 0)))
	tt.Equal(t, `((quux :file "testdata" :files ("sys-test" "sub")))`,
		slip.ObjectString(sys.Receive(scope, ":depends-on", nil, 0)))
	tt.Equal(t, `((:sample (+ (sys-test) six)) (:just-eval 3))`,
		slip.ObjectString(sys.Receive(scope, ":in-order-to", nil, 0)))
}
