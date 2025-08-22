// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"crypto"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CryptoHash{Function: slip.Function{Name: "crypto-hash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "crypto-hash",
			Args: []*slip.DocArg{
				{
					Name: "data",
					Type: "octets",
					Text: "The data to hashed. Must be coercible to _octets_.",
				},
				{
					Name: "method",
					Type: "symbol",
					Text: "The hash method..",
				},
			},
			Return: "octets",
			Text: `__crypto-hash__ returns a hash of the provided _data_ according to the _method_.
The _method_ must be one of:
  :md5
  :sha1
  :sha224
  :sha256
  :sha384
  :sha512
  :sha3-224
  :sha3-256
  :sha3-384
  :sha3-512
  :sha512-224
  :sha512-256
`,
			Examples: []string{
				`(crypto-hash "some data" :sha256) => TBD`,
			},
		}, &Pkg)
}

// CryptoHash represents the crypto-hash function.
type CryptoHash struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CryptoHash) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	data := []byte(slip.CoerceToOctets(args[0]).(slip.Octets))
	var h crypto.Hash
	switch args[1] {
	case slip.Symbol(":md5"):
		h = crypto.MD5
	case slip.Symbol(":sha1"):
		h = crypto.SHA1
	case slip.Symbol(":sha224"):
		h = crypto.SHA224
	case slip.Symbol(":sha256"):
		h = crypto.SHA256
	case slip.Symbol(":sha384"):
		h = crypto.SHA384
	case slip.Symbol(":sha512"):
		h = crypto.SHA512
	case slip.Symbol(":sha3-224"):
		h = crypto.SHA3_224
	case slip.Symbol(":sha3-256"):
		h = crypto.SHA3_256
	case slip.Symbol(":sha3-384"):
		h = crypto.SHA3_384
	case slip.Symbol(":sha3-512"):
		h = crypto.SHA3_512
	case slip.Symbol(":sha512-224"):
		h = crypto.SHA512_224
	case slip.Symbol(":sha512-256"):
		h = crypto.SHA512_256
	default:
		slip.TypePanic(s, depth, "method", args[1],
			":md5",
			":sha1",
			":sha224",
			":sha256",
			":sha384",
			":sha512",
			":sha3-224",
			":sha3-256",
			":sha3-384",
			":sha3-512",
			":sha512-224",
			":sha512-256",
		)
	}
	return slip.Octets(h.New().Sum(data))
}
