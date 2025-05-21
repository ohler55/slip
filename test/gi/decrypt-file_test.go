// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDecryptFileOk(t *testing.T) {
	err := os.WriteFile("testdata/enc-input.txt", []byte("some data"), 0666)
	tt.Nil(t, err)
	(&sliptest.Function{
		Source: `(progn
                  (encrypt-file "testdata/enc-input.txt" "testdata/enc-output.enc"
                                'quux :cipher :des :pad #\space :nonce "12345678")
                  (decrypt-file "testdata/enc-output.enc" "testdata/dec-output.txt"
                                'quux :cipher :des :strip #\space :perm #o776))`,
		Expect: "nil",
	}).Test(t)
	var content []byte
	content, err = os.ReadFile("testdata/dec-output.txt")
	tt.Nil(t, err)
	tt.Equal(t, "some data", string(content))
}

func TestDecryptFileNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(decrypt-file "testdata/not-found" "testdata/enc-output.txt" 'quux)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestDecryptFileWriteError(t *testing.T) {
	err := os.WriteFile("testdata/enc-input.txt", []byte("some data"), 0666)
	tt.Nil(t, err)
	(&sliptest.Function{
		Source: `(progn
                  (encrypt-file "testdata/enc-input.txt" "testdata/enc-output.enc"
                                'quux :cipher :des :pad #\space :nonce "12345678")
                  (decrypt-file "testdata/enc-output.enc" "nothing/there" 'quux :cipher :des))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestDecryptFileShort(t *testing.T) {
	err := os.WriteFile("testdata/enc-input.enc", []byte("1234567"), 0666)
	tt.Nil(t, err)
	(&sliptest.Function{
		Source:    `(decrypt-file "testdata/enc-input.enc" "testdata/dec-output.txt" 'quux :cipher :des )`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
