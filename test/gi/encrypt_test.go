// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEncryptDES(t *testing.T) {
	(&sliptest.Function{
		Source: `(encrypt "some data" 'quux :cipher :des :nonce "1234567890123456" :pad #\space)`,
		Array:  true,
		Expect: `#(49 50 51 52 53 54 55 56 236 185 245 197 124 90 196 125 73 174 225 183 50 126 42
    228)`,
	}).Test(t)
}

func TestEncryptAES16(t *testing.T) {
	(&sliptest.Function{
		Source: `(encrypt "some data" 'quux1234567890ab :nonce "1234567890123456" :pad #\space)`,
		Array:  true,
		Expect: `#(49 50 51 52 53 54 55 56 57 48 49 50 51 52 53 54 248 164 140 225 191 235 157 52
    89 100 138 179 244 91 87 216)`,
	}).Test(t)
}

func TestEncryptAESSmall(t *testing.T) {
	(&sliptest.Function{
		Source: `(encrypt "some data" 'quux :nonce "1234567890123456" :pad #\space)`,
		Array:  true,
		Expect: `#(49 50 51 52 53 54 55 56 57 48 49 50 51 52 53 54 207 183 254 95 157 187 77 54
    194 145 12 89 105 104 65 225)`,
	}).Test(t)
}

func TestEncryptAESMedium(t *testing.T) {
	(&sliptest.Function{
		Source: `(encrypt "some data" 'quux1234567890abcdef :nonce "1234567890123456" :pad #\space)`,
		Array:  true,
		Expect: `#(49 50 51 52 53 54 55 56 57 48 49 50 51 52 53 54 116 148 251 104 190 13 253 125
    75 228 107 157 158 200 72 218)`,
	}).Test(t)
}

func TestEncryptAESLarge(t *testing.T) {
	(&sliptest.Function{
		Source: `(encrypt "some data" 'quux1234567890abcdef12345678 :nonce "1234567890123456" :pad #\space)`,
		Array:  true,
		Expect: `#(49 50 51 52 53 54 55 56 57 48 49 50 51 52 53 54 29 108 69 208 5 77 40 108 116
    161 54 40 44 148 80 85)`,
	}).Test(t)
}

func TestEncryptAESMax(t *testing.T) {
	(&sliptest.Function{
		Source: `(encrypt "some data" 'quux1234567890abcdef1234567890abcdef :nonce "1234567890123456" :pad #\space)`,
		Array:  true,
		Expect: `#(49 50 51 52 53 54 55 56 57 48 49 50 51 52 53 54 75 170 238 127 160 84 118 51 46
    47 4 186 69 22 72 76)`,
	}).Test(t)
}

func TestEncryptAES(t *testing.T) {
	(&sliptest.Function{
		Source: `(length (encrypt "some data" 'quux))`,
		Expect: `32`,
	}).Test(t)
}

func TestEncryptBadData(t *testing.T) {
	(&sliptest.Function{
		Source:    `(encrypt t 'quux)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestEncryptBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    `(encrypt 'quux t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestEncryptBadCipher(t *testing.T) {
	(&sliptest.Function{
		Source:    `(encrypt "some data" 'quux :cipher :bad)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
