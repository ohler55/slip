// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"os"
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFileStringLengthStream(t *testing.T) {
	defer func() {
		_ = os.RemoveAll("testdata/remove-me")
	}()
	(&sliptest.Function{
		Source: `(with-open-file
                  (file "testdata/remove-me" :direction :output :if-does-not-exist :create)
                  (file-string-length file "abc"))`,
		Expect: "3",
	}).Test(t)
}

func TestFileStringLengthNilStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-string-length nil 123)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(file-string-length nil "𝄢𝄢")`,
		Expect: "8",
	}).Test(t)
}

func TestFileStringLengthCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-string-length nil #\X)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(file-string-length nil #\𝄢)`,
		Expect: "4",
	}).Test(t)
}

func TestFileStringLengthNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-string-length t "abc")`,
		Panics: true,
	}).Test(t)
}
