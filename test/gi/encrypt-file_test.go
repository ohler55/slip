// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEncryptFileOk(t *testing.T) {
	err := os.WriteFile("testdata/enc-input.txt", []byte("some data"), 0666)
	tt.Nil(t, err)
	(&sliptest.Function{
		Source: `(encrypt-file "testdata/enc-input.txt" "testdata/enc-output.enc"
                               'quux :cipher :des :pad #\space :nonce "12345678")`,
		Expect: "nil",
	}).Test(t)
	var content []byte
	content, err = os.ReadFile("testdata/enc-output.enc")
	tt.Nil(t, err)
	tt.Equal(t,
		[]byte{49, 50, 51, 52, 53, 54, 55, 56, 236, 185, 245, 197, 124,
			90, 196, 125, 73, 174, 225, 183, 50, 126, 42, 228},
		content)
}

func TestEncryptFileRand(t *testing.T) {
	err := os.WriteFile("testdata/enc-input.txt", []byte("some data"), 0666)
	tt.Nil(t, err)
	(&sliptest.Function{
		Source: `(encrypt-file "testdata/enc-input.txt" "testdata/enc-output.enc"
                               'quux :cipher :des :pad #\space :perm #o776)`,
		Expect: "nil",
	}).Test(t)
	var content []byte
	content, err = os.ReadFile("testdata/enc-output.enc")
	tt.Nil(t, err)
	// Will be checked in the decrypt-file test.
	tt.Equal(t, 24, len(content))
}

func TestEncryptFileNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(encrypt-file "testdata/not-found" "testdata/enc-output.enc" 'quux)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestEncryptFileWriteError(t *testing.T) {
	err := os.WriteFile("testdata/enc-input.txt", []byte("some data"), 0666)
	tt.Nil(t, err)
	(&sliptest.Function{
		Source:    `(encrypt-file "testdata/enc-input.txt" "nothing/there" 'quux)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
