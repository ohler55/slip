// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"fmt"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSaveBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save '(nil a 2 2.5) out)
                  (get-output-stream-string out))`,
		Expect: `"(nil a 2 2.5d+00)"`,
	}).Test(t)
}

func TestSaveSymbols(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save '(t || :key |a;b|) out)
                  (get-output-stream-string out))`,
		Expect: `"(t || :key |a;b|)"`,
	}).Test(t)
}

func TestSaveString(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save '("abc" . "def") out)
                  (get-output-stream-string out))`,
		Expect: `"("abc" . "def")"`,
	}).Test(t)
}

func TestSaveOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save (coerce "abc" 'octets) out)
                  (get-output-stream-string out))`,
		Expect: `"#(97 98 99)"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save (coerce "" 'octets) out)
                  (get-output-stream-string out))`,
		Expect: `"#()"`,
	}).Test(t)
}

func TestSaveList(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save '(() (a) ((b))) out)
                  (get-output-stream-string out))`,
		Expect: `"(nil (a) ((b)))"`,
	}).Test(t)
}

func TestSaveTime(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save '(@2024-06-21T01:02:03Z) out)
                  (get-output-stream-string out))`,
		Expect: `"(@2024-06-21T01:02:03Z)"`,
	}).Test(t)
}

func TestSaveNumbers(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save '(1 2.5s-1 3.5d-1 4.5L-1 3/2 4/2 123456789012345678901234567890) out)
                  (get-output-stream-string out))`,
		Expect: `"(1 2.5s-01 3.5d-01 4.5L-01 3/2 2 123456789012345678901234567890)"`,
	}).Test(t)
}

func TestSaveLambda(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save (lambda (x) (+ 2 x)) out)
                  (get-output-stream-string out))`,
		Expect: `"(lambda (x) (+ 2 x))"`,
	}).Test(t)
}

func TestSaveCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save '(#\A #\u0042 #\Space #\u0001) out)
                  (get-output-stream-string out))`,
		Expect: `"(#\A #\B #\Space #\u0001)"`,
	}).Test(t)
}

func TestSaveArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save '(#0A() #() #(1 2 3) #2A((a b)(c d))) out)
                  (get-output-stream-string out))`,
		Expect: `"(#0Anil #() #(1 2 3) #2A((a b) (c d)))"`,
	}).Test(t)
}

func TestSaveBitVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save #*1010 out)
                  (get-output-stream-string out))`,
		Expect: `"#*1010"`,
	}).Test(t)
}

func TestSaveBufferSize(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save '(nil abc 123) out :buffer-size 4)
                  (get-output-stream-string out))`,
		Expect: `"(nil abc 123)"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save '(nil abc 123) out :buffer-size t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSaveNotReadable(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (save *cl* out))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSaveNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(save 'quux t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

type badWriter int

func (w badWriter) Write([]byte) (int, error) {
	return 0, fmt.Errorf("failed")
}

func TestSaveWriteFail(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(save 'quux out)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(save 'quux out :buffer-size 2)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
