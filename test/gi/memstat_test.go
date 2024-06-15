// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMemstat(t *testing.T) {
	(&sliptest.Function{
		Source: `(memstat)`,
		Validate: func(t *testing.T, v slip.Object) {
			alist := v.(slip.List)
			tt.Equal(t, true, 16 < len(alist))
			a, ok := alist[0].(slip.List)
			tt.Equal(t, true, ok)
			tt.Equal(t, 2, len(a))
			_, ok = a[0].(slip.Symbol)
			tt.Equal(t, true, ok)
		},
	}).Test(t)
}
