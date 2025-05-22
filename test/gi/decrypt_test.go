// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDecryptDES(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((x #(49 50 51 52 53 54 55 56 236 185 245 197 124 90 196 125 73 174 225 183 50 126 42 228)))
  (list
   (decrypt x 'quux :cipher :des :strip #\space)
   (aref x (1- (length x)))))`,
		Array:  true,
		Expect: `(#(115 111 109 101 32 100 97 116 97) 228)`,
	}).Test(t)
	// Verify octets data input is not modified.
	(&sliptest.Function{
		Source: `
(let ((x (coerce #(49 50 51 52 53 54 55 56 236 185 245 197 124 90 196 125 73 174 225 183 50 126 42 228) 'octets)))
  (list
   (decrypt x 'quux :cipher :des :strip #\space)
   (aref x (1- (length x)))))`,
		Array:  true,
		Expect: `(#(115 111 109 101 32 100 97 116 97) 228)`,
	}).Test(t)
}

func TestDecryptAES16(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((x
       #(49 50 51 52 53 54 55 56 57 48 49 50 51 52 53 54 248 164 140 225 191 235 157 52 89 100 138 179 244 91 87 216)))
  (decrypt x 'quux1234567890ab :strip #\space))`,
		Array:  true,
		Expect: `#(115 111 109 101 32 100 97 116 97)`,
	}).Test(t)
}

func TestDecryptAESSmall(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((x
       #(49 50 51 52 53 54 55 56 57 48 49 50 51 52 53 54 207 183 254 95 157 187 77 54 194 145 12 89 105 104 65 225)))
  (decrypt x 'quux :strip #\space))`,
		Array:  true,
		Expect: `#(115 111 109 101 32 100 97 116 97)`,
	}).Test(t)
}

func TestDecryptAESMedium(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((x
       #(49 50 51 52 53 54 55 56 57 48 49 50 51 52 53 54 116 148 251 104 190 13 253 125 75 228 107 157 158 200 72 218)))
  (decrypt x 'quux1234567890abcdef :strip #\space))`,
		Array:  true,
		Expect: `#(115 111 109 101 32 100 97 116 97)`,
	}).Test(t)
}

func TestDecryptAESLarge(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((x
       #(49 50 51 52 53 54 55 56 57 48 49 50 51 52 53 54 29 108 69 208 5 77 40 108 116 161 54 40 44 148 80 85)))
  (decrypt x 'quux1234567890abcdef12345678 :strip #\space))`,
		Array:  true,
		Expect: `#(115 111 109 101 32 100 97 116 97)`,
	}).Test(t)
}

func TestDecryptAESMax(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((x
       #(49 50 51 52 53 54 55 56 57 48 49 50 51 52 53 54 75 170 238 127 160 84 118 51 46 47 4 186 69 22 72 76)))
  (decrypt x 'quux1234567890abcdef1234567890abcdef :strip #\space))`,
		Array:  true,
		Expect: `#(115 111 109 101 32 100 97 116 97)`,
	}).Test(t)
}

func TestDecryptBadData(t *testing.T) {
	(&sliptest.Function{
		Source:    `(decrypt t 'quux)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestDecryptBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    `(decrypt 'quux t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestDecryptBadCipher(t *testing.T) {
	(&sliptest.Function{
		Source:    `(decrypt "some data" 'quux :cipher :bad)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDecryptShort(t *testing.T) {
	(&sliptest.Function{
		Source:    `(decrypt #(49 50 51 52 53 54 55 56) 'quux)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
