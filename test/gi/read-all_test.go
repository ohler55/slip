// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

type badReader int

func (w badReader) Read([]byte) (int, error) {
	return 0, fmt.Errorf("oops")
}

func TestReadAllStream(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader("abc\ndef\n"))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-all in)`,
		Expect: `"abc
def
"`,
	}).Test(t)
}

func TestReadAllNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-all t)`,
		Panics: true,
	}).Test(t)
}

func TestReadAllError(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), slip.NewInputStream(badReader(0)))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-all in)`,
		Panics: true,
	}).Test(t)
}
