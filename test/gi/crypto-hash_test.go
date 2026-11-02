// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCryptoHashMD5(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :md5))`,
		Expect: `"HlAhCgICSX+3m8OLat5sNA=="`,
	}).Test(t)
}

func TestCryptoHashSHA1(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha1))`,
		Expect: `"uvNFUf7LSKzD2oaOuF4bbayd41Y="`,
	}).Test(t)
}

func TestCryptoHashSHA224(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha224))`,
		Expect: `"mzpf7INPIMYQQDeCAkrBnrLH/shTfOjoiMGSCw=="`,
	}).Test(t)
}

func TestCryptoHashSHA256(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha256))`,
		Expect: `"EweZDmulyhRes16ZGCqb7EZTG8VN32VqYCx4D6AkDe4="`,
	}).Test(t)
}

func TestCryptoHashSHA384(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha384))`,
		Expect: `"qcYaFi9LVypj5rDitFrvRztzAn1ZBVWWakwJGFg3/3KhAZHBNuw/RhTXkU0dqCPw"`,
	}).Test(t)
}

func TestCryptoHashSHA512(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha512))`,
		Expect: `"4WRedJLwMvtixnTbdVAL57Jgv8DaqWWCHds/ikm10zeI7j8EZ0TiuVr7XD2PJQDFScqJ15/GiQiF0o4FUAdCTw=="`,
	}).Test(t)
}

func TestCryptoHashSHA3_224(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha3-224))`,
		Expect: `"X9nfFY/X63N4kzMsXhmQbY0TUqaTLb8QwCALUw=="`,
	}).Test(t)
}

func TestCryptoHashSHA3_256(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha3-256))`,
		Expect: `"WTPpJzFmk0puuq9YFxB0R5MJp96EYHxFCwHCZbMIFxI="`,
	}).Test(t)
}

func TestCryptoHashSHA3_384(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha3-384))`,
		Expect: `"cg14BHysvF+5LmXT/OewHyFmsjqahDxRbdDO6EWIL3AubKVfgdtCINOgM/dmYur+"`,
	}).Test(t)
}

func TestCryptoHashSHA3_512(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha3-512))`,
		Expect: `"I8jd37PveuXWQRqmY2PBaKP6PXYqqqPU0zomNJLRl+7CpBIpdNyVDXPmPyFi/iJnRD5FP2H4t/A39UJQCbvIKw=="`,
	}).Test(t)
}

func TestCryptoHashSHA512_224(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha512-224))`,
		Expect: `"01QCWwgp/ozW7lT0Qdd4fk6kPjvomgVEvs+kgg=="`,
	}).Test(t)
}

func TestCryptoHashSHA512_256(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha512-256))`,
		Expect: `"dgukJ16Engd3UH21jw10/AWzDhDMwnqZ1QRl9bZjkCY="`,
	}).Test(t)
}

// FIPS 180-2 test vector: SHA-256("abc") = ba7816bf...f20015ad.
func TestCryptoHashSHA256Vector(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "abc" :sha256))`,
		Expect: `"ungWv48Bz+pBQUDeXa4iI7ADYaOWF3qctBD/YfIAFa0="`,
	}).Test(t)
}

func TestCryptoHashOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash (coerce '(97 98 99) 'octets) :sha256))`,
		Expect: `"ungWv48Bz+pBQUDeXa4iI7ADYaOWF3qctBD/YfIAFa0="`,
	}).Test(t)
}

func TestCryptoHashBadMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(base64-encode (crypto-hash "some data" :bad))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
