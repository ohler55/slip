// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/des"
	"crypto/sha3"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Decrypt{Function: slip.Function{Name: "decrypt", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "decrypt",
			Args: []*slip.DocArg{
				{
					Name: "data",
					Type: "octets",
					Text: "The data to decrypt. Must be coercible to _octets_.",
				},
				{
					Name: "key",
					Type: "string",
					Text: "The key to use for decryption. Must be coercible to _octets_.",
				},
				{
					Name: "&key"},
				{
					Name: "cipher",
					Type: "symbol",
					Text: "The decryption algorithm; :aes or :des",
				},
				{
					Name: "strip",
					Type: "octet",
					Text: "If supplied any trailing octets that match are trimmed after decrypting.",
				},
			},
			Return: "octets",
			Text: `__decrypt__ the _data_ using _key_ providing according to the _cipher_.
If _key_ is not a supported block size it is hashed before being used just as __encrypt__ does.`,
			Examples: []string{
				`(decrypt`,
				` #(49 50 51 52 53 54 55 56 236 185 245 197 124 90 196 125 71 124 207 160 115 152 186 210)`,
				` 'quux :cipher :des :strip 0) => #(115 111 109 101 32 100 97 116 97) ;; "some data"`,
			},
		}, &Pkg)
}

// Decrypt represents the decrypt function.
type Decrypt struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Decrypt) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 6)
	var (
		non []byte
		dup bool
	)
	if _, ok := args[0].(slip.Octets); ok {
		dup = true
	}
	data := []byte(slip.CoerceToOctets(args[0]).(slip.Octets))
	strip, block, bsize, trim := extractDencryptArgs(args[1:])

	if len(data) < bsize {
		slip.NewPanic("decrypt data is too short")
	}
	non = data[:bsize]
	data = data[bsize:]
	if dup {
		tmp := make([]byte, len(data))
		copy(tmp, data)
		data = tmp
	}
	mode := cipher.NewCBCDecrypter(block, non)
	mode.CryptBlocks(data, data)
	if trim {
		data = bytes.TrimRight(data, string([]byte{strip}))
	}
	return slip.Octets(data)
}

func extractDencryptArgs(args slip.List) (strip byte, block cipher.Block, bsize int, trim bool) {
	var ciph slip.Object = slip.Symbol(":aes")

	key := []byte(slip.CoerceToOctets(args[0]).(slip.Octets))
	strip = byte(0)
	if 1 < len(args) {
		rest := args[1:]
		if obj, has := slip.GetArgsKeyValue(rest, slip.Symbol(":cipher")); has {
			ciph = obj
		}
		if obj, has := slip.GetArgsKeyValue(rest, slip.Symbol(":strip")); has {
			strip = byte(slip.ToOctet(obj).(slip.Octet))
			trim = true
		}
	}
	klen := len(key)
	switch ciph {
	case slip.Symbol(":aes"):
		switch {
		case klen == 16 || klen == 24 || klen == 32:
			// Leave key as is.
		case klen <= 16:
			key = sha3.SumSHAKE256(key, 16)
		case klen <= 24:
			key = sha3.SumSHAKE256(key, 24)
		default:
			key = sha3.SumSHAKE256(key, 32)
		}
		block, _ = aes.NewCipher(key)
		bsize = aes.BlockSize
	case slip.Symbol(":des"):
		if klen != 8 {
			key = sha3.SumSHAKE256(key, 8)
		}
		block, _ = des.NewCipher(key)
		bsize = des.BlockSize
	default:
		slip.PanicType("cipher", ciph, ":aes", ":des")
	}

	return
}
