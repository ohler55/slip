// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestBagWriteOk(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("*print-pretty*"), slip.True)
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag)`,
		Expect: `"{a: 7}"`,
	}).Test(t)
}

func TestBagWriteStreamNil(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("*print-pretty*"), slip.True)
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag nil)`,
		Expect: `"{a: 7}"`,
	}).Test(t)
}

func TestBagWriteStream(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("*print-pretty*"), slip.True)
	var b bytes.Buffer
	slip.SetVar(slip.Symbol("buf"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag buf)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "{a: 7}", b.String())
}

func TestBagWriteStreamT(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("*print-pretty*"), slip.True)
	var b bytes.Buffer
	orig, _ := slip.GetVar(slip.Symbol("*standard-output*"))
	slip.SetVar(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &b})
	defer slip.SetVar(slip.Symbol("*standard-output*"), orig)
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "{a: 7}", b.String())
}

func TestBagWritePretty(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("*print-pretty*"), nil)
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag :pretty t)`,
		Expect: `"{a: 7}"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag :pretty nil :depth 0)`,
		Expect: `"{a:7}"`,
	}).Test(t)
}

func TestBagWriteDepth(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("*print-pretty*"), nil)
	_ = slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "[1 [2 [3 4] 5] 6]"))`).Eval(scope).(*flavors.Instance)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag :pretty t)`,
		Expect: `"[1 [2 [3 4] 5] 6]"`,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag :pretty t :depth 3)`,
		Expect: `"[
  1
  [2 [3 4] 5]
  6
]"`,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag :pretty t :depth 1 :json t)`,
		Expect: `"[
  1,
  [
    2,
    [
      3,
      4
    ],
    5
  ],
  6
]"`,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag :pretty t :depth 0 :json t)`,
		Expect: `"[1,[2,[3,4],5],6]"`,
	}).Test(t)
}

func TestBagWriteRightMargin(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("*print-pretty*"), nil)
	_ = slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "[1 [2 [3 4] 5] 6]"))`).Eval(scope).(*flavors.Instance)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag :pretty t :right-margin 16)`,
		Expect: `"[
  1
  [2 [3 4] 5]
  6
]"`,
	}).Test(t)
}

func TestBagWriteColor(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("*print-pretty*"), nil)
	_ = slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "[1 2 3]"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag :pretty t :color t)`,
		Expect: "\"\x1b[m[\x1b[36m1 \x1b[36m2 \x1b[36m3\x1b[m]\"",
	}).Test(t)
}

func TestBagWriteTime(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("*print-pretty*"), slip.True)
	_ = slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :set '(@2022-09-24T13:49:55Z)))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag :time-format "2006-01-02" :time-wrap "time")`,
		Expect: `"[{time:"2022-09-24"}]"`,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag :time-format nil :time-wrap nil)`,
		Expect: `"[1664027395000000000]"`,
	}).Test(t)
}

func TestBagWriteNotBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-write t)`,
		Panics: true,
	}).Test(t)
}

func TestBagWriteArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-write)`,
		Panics: true,
	}).Test(t)
}

func TestBagWriteBadStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-write (make-instance 'bag-flavor) 7)`,
		Panics: true,
	}).Test(t)
}

func TestBagWriteBadDepth(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-write (make-instance 'bag-flavor) :depth t)`,
		Panics: true,
	}).Test(t)
}

func TestBagWriteBadRightMargin(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-write (make-instance 'bag-flavor) :right-margin t)`,
		Panics: true,
	}).Test(t)
}

func TestBagWriteBadTimeFormat(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-write (make-instance 'bag-flavor) :time-format t)`,
		Panics: true,
	}).Test(t)
}

func TestBagWriteBadTimeWrap(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-write (make-instance 'bag-flavor) :time-wrap t)`,
		Panics: true,
	}).Test(t)
}

func TestBagWriteBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-write (make-instance 'bag-flavor) :bad t)`,
		Panics: true,
	}).Test(t)
}

type badWriter int

func (w badWriter) Write([]byte) (int, error) {
	return 0, fmt.Errorf("failed")
}

func TestBagWriteBadWriter(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("buf"), &slip.OutputStream{Writer: badWriter(0)})
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-write bag buf)`,
		Panics: true,
	}).Test(t)
}
