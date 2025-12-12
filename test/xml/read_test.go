// Copyright (c) 2023, Peter Ohler, All rights reserved.

package xml_test

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

func TestPackage(t *testing.T) {
	(&sliptest.Function{
		Source: `*xml*`,
		Expect: "#<package xml>",
	}).Test(t)
}

func TestReadStream(t *testing.T) {
	sample := `<?xml version="1.0"?>
<!DOCTYPE sample PUBLIC "sample.dtd">
<top id="123">
  <child>Some text.</child>
  <!--a comment-->
  <blank/>
</top>
`
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader(sample))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(xml-read in)`,
		Expect: `((:processing-instruction "xml" "version="1.0"")
 (:directive "DOCTYPE sample PUBLIC "sample.dtd"")
 ("top" (("id" . "123")) ("child" () "Some text.") (:comment "a comment")
        ("blank" ())))`,
	}).Test(t)
}

func TestReadString(t *testing.T) {
	(&sliptest.Function{
		Source: `(xml-read "<top id=\"123\"><child>Some text.</child></top>")`,
		Expect: `("top" (("id" . "123")) ("child" () "Some text."))`,
	}).Test(t)
}

func TestReadHTML(t *testing.T) {
	(&sliptest.Function{
		Source: `(xml-read "<!DOCTYPE html><html><body><p>Hello.</p></body></html>" :html t)`,
		Expect: `((:directive "DOCTYPE html") ("html" () ("body" () ("p" () "Hello."))))`,
	}).Test(t)
}

func TestReadNoTrim(t *testing.T) {
	(&sliptest.Function{
		Source: `(xml-read "<top id=\"123\"> <child>Some text. </child></top>" :trim nil :strict nil)`,
		Expect: `("top" (("id" . "123")) " " ("child" () "Some text. "))`,
	}).Test(t)
}

func TestReadBadInput(t *testing.T) {
	(&sliptest.Function{
		Source:    `(xml-read t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestReadBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    `(xml-read "<top/>" :bad t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-read "<top/>" t t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-read "<top/>" :strict)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestReadError(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(badReader(0))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(xml-read in)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}
