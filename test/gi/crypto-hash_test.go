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
		Expect: `"c29tZSBkYXRh1B2M2Y8AsgTpgAmY7PhCfg=="`,
	}).Test(t)
}

func TestCryptoHashSHA1(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha1))`,
		Expect: `"c29tZSBkYXRh2jmj7l5rSw0yVb/vlWAYkK/YBwk="`,
	}).Test(t)
}

func TestCryptoHashSHA224(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha224))`,
		Expect: `"c29tZSBkYXRh0UoCjCo6K8lHYQK7KII0xBWisB+CjqYqxbPkLw=="`,
	}).Test(t)
}

func TestCryptoHashSHA256(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha256))`,
		Expect: `"c29tZSBkYXRh47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="`,
	}).Test(t)
}

func TestCryptoHashSHA384(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha384))`,
		Expect: `"c29tZSBkYXRhOLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"`,
	}).Test(t)
}

func TestCryptoHashSHA512(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha512))`,
		Expect: `"c29tZSBkYXRhz4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="`,
	}).Test(t)
}

func TestCryptoHashSHA3_224(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha3-224))`,
		Expect: `"c29tZSBkYXRha04DQjZn27c7bhVFTw6xq9RZf5obB44/W1prxw=="`,
	}).Test(t)
}

func TestCryptoHashSHA3_256(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha3-256))`,
		Expect: `"c29tZSBkYXRhp//G+L8e12ZRwUdWoGHWYvWA/03kO0n6gtgKS4D4Q0o="`,
	}).Test(t)
}

func TestCryptoHashSHA3_384(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha3-384))`,
		Expect: `"c29tZSBkYXRhDGOnW4ReT30BEH2FLkwkhcUaUKqqlPxhmV5xu+6YOirDcTgxJkrbR/tr0eBY1fAE"`,
	}).Test(t)
}

func TestCryptoHashSHA3_512(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha3-512))`,
		Expect: `"c29tZSBkYXRhpp9zzKI6msXItWfcGFp1bpfJghZP4lhZ4NHcwUdcgKYVshI68fX5TBHj6UAsOsVY9QAZnZW20+MBdYWGKB3NJg=="`,
	}).Test(t)
}

func TestCryptoHashSHA512_224(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha512-224))`,
		Expect: `"c29tZSBkYXRhbtDdAoBvqJ4l3gYMGdOshsq7h9ag3dBcMzuE9A=="`,
	}).Test(t)
}

func TestCryptoHashSHA512_256(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (crypto-hash "some data" :sha512-256))`,
		Expect: `"c29tZSBkYXRhxnK40e9W7Sirh8NiLFEUBpvdOte4+XN0mNDAHs7wlno="`,
	}).Test(t)
}

func TestCryptoHashBadMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(base64-encode (crypto-hash "some data" :bad))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
