// Copyright (c) 2026, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReadSequenceBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcdefghi" 'octets)))
                      (with-input-from-string (s "DEF")
                        (list (read-sequence seq s) (coerce seq 'string))))`,
		Expect: `(3 "DEFdefghi")`,
	}).Test(t)
}

func TestReadSequenceStartNilEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcdefghi" 'octets)))
                      (with-input-from-string (s "DEF")
                        (read-sequence seq s :start 3 :end nil))
                      (coerce seq 'string))`,
		Expect: `"abcDEFghi"`,
	}).Test(t)
}

func TestReadSequenceNilStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcdefghi" 'octets)))
                      (with-input-from-string (s "DEF")
                        (read-sequence seq s :start nil :end 2))
                      (coerce seq 'string))`,
		Expect: `"DEcdefghi"`,
	}).Test(t)
}

func TestReadSequenceBadSequence(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-input-from-string (s "DEF")
                   (read-sequence "xyz" s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReadSequenceBadStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(read-sequence (coerce "abcdefghi" 'octets) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReadSequenceBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcdefghi" 'octets)))
                      (with-input-from-string (s "DEF")
                        (read-sequence seq s :start t :end 2)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcdefghi" 'octets)))
                      (with-input-from-string (s "DEF")
                        (read-sequence seq s :start -1 :end 2)))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcdefghi" 'octets)))
                      (with-input-from-string (s "DEF")
                        (read-sequence seq s :start 10)))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
func TestReadSequenceBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcdefghi" 'octets)))
                      (with-input-from-string (s "DEF")
                        (read-sequence seq s :end -1)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcdefghi" 'octets)))
                      (with-input-from-string (s "DEF")
                        (read-sequence seq s :end t)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((seq (coerce "abcdefghi" 'octets)))
                      (with-input-from-string (s "DEF")
                        (read-sequence seq s :end 10)))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestReadSequenceReadError(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), slip.NewInputStream(badReader(0)))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((seq (coerce "abcdefghi" 'octets)))
                   (read-sequence seq in))`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}
