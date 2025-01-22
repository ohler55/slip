// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFileInfoFile(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-info "testdata/comp.lisp")`,
		Validate: func(t *testing.T, v slip.Object) {
			plist := v.(slip.List)
			tt.Equal(t, slip.String("comp.lisp"), pget(":name", plist))
			ss := pget(":path", plist).(slip.String)
			tt.Equal(t, "/comp.lisp$/", string(ss))
			tt.Equal(t, slip.String("-rw-r--r--"), pget(":perm", plist))
			tt.Equal(t, slip.Symbol(":regular"), pget(":type", plist))
			tt.Equal(t, slip.Fixnum(28), pget(":size", plist))
			tt.Equal(t, nil, pget(":is-dir", plist))
			_, ok := pget(":mod-time", plist).(slip.Time)
			tt.Equal(t, true, ok)
		},
	}).Test(t)
}

func TestFileInfoDir(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-info "testdata")`,
		Validate: func(t *testing.T, v slip.Object) {
			plist := v.(slip.List)
			tt.Equal(t, slip.String("testdata"), pget(":name", plist))
			tt.Equal(t, slip.Symbol(":directory"), pget(":type", plist))
			tt.Equal(t, slip.True, pget(":is-dir", plist))
		},
	}).Test(t)
}

func TestFileInfoNotFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-info "quux.lisp")`,
		Expect: "nil",
	}).Test(t)
}

func TestFileInfoNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(file-info t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func pget(key string, plist slip.List) slip.Object {
	k := slip.Symbol(key)
	for i := 0; i < len(plist); i += 2 {
		if k == plist[i] {
			return plist[i+1]
		}
	}
	return nil
}
