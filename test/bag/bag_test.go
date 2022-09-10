// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
)

func TestBag(t *testing.T) {
	tt.NotNil(t, bag.Flavor())
	scope := slip.NewScope()

	_ = slip.ReadString("(setq data (make-instance 'bag-flavor))").Eval(scope)

	// Verify bag-flavor inherits from vanilla-flavor
	result := slip.ReadString("(send data :operation-handler-p :describe)").Eval(scope)
	tt.Equal(t, slip.True, result)

	fmt.Printf("*** result: %s\n", result)

	result = slip.ReadString("(send data :describe)").Eval(scope)
	// tt.Equal(t, "(bag-flavor vanilla-flavor)", names.String())

	fmt.Printf("*** result: %s\n", result)
}
