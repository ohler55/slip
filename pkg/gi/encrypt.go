// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/des"
	"crypto/rand"
	"crypto/sha3"
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Encrypt{Function: slip.Function{Name: "encrypt", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "encrypt",
			Args: []*slip.DocArg{
				{
					Name: "data",
					Type: "octets",
					Text: "The data to encrypt. Must be coercible to _octets_.",
				},
				{
					Name: "key",
					Type: "string",
					Text: "The key to use for the encryption. Must be coercible to _octets_.",
				},
				{
					Name: "&key"},
				{
					Name: "cipher",
					Type: "symbol",
					Text: "The encryption algorithm; :aes or :des",
				},
				{
					Name: "nonce",
					Type: "string",
					Text: "The random nonce to use. Default it to randomly generate a nonce",
				},
				{
					Name: "pad",
					Type: "octet",
					Text: "If the data needs to be padded to fill out a block this octet is used. Default is #x0",
				},
			},
			Return: "octets",
			Text: `__encrypt__ the _data_ using _key_ providing according to the _method_. Note
that data that is less than a multiple of the cipher block size will be padded with _:pad_ octets.
If _key_ is not a supported block size it is hashed before being used.`,
			Examples: []string{
				`(encrypt "some data" 'quux :cipher :des :nonce "1234567890123456") =>`,
				`  #(49 50 51 52 53 54 55 56 236 185 245 197 124 90 196 125 71 124 207 160 115 152 186 210)`,
			},
		}, &Pkg)
}

// Encrypt represents the encrypt function.
type Encrypt struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Encrypt) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 8)
	data := []byte(slip.CoerceToOctets(args[0]).(slip.Octets))
	non, pad, block, bsize := extractEncryptArgs(args[1:])
	// Data must be a multiple of the block size so pad as needed.
	if len(data)%bsize != 0 {
		data = append(data, bytes.Repeat([]byte{pad}, bsize-len(data)%bsize)...)
	}
	buf := make([]byte, bsize+len(data))
	nonce := buf[:bsize]
	if 0 < len(non) {
		copy(nonce, non)
	} else {
		_, _ = io.ReadFull(rand.Reader, nonce)
	}
	mode := cipher.NewCBCEncrypter(block, nonce)
	mode.CryptBlocks(buf[bsize:], data)

	return slip.Octets(buf)
}

func extractEncryptArgs(args slip.List) (nonce []byte, pad byte, block cipher.Block, bsize int) {
	key := []byte(slip.CoerceToOctets(args[0]).(slip.Octets))
	var ciph slip.Object = slip.Symbol(":aes")
	pad = byte(0)
	if 1 < len(args) {
		rest := args[1:]
		if obj, has := slip.GetArgsKeyValue(rest, slip.Symbol(":cipher")); has {
			ciph = obj
		}
		if obj, has := slip.GetArgsKeyValue(rest, slip.Symbol(":nonce")); has {
			nonce = []byte(slip.CoerceToOctets(obj).(slip.Octets))
		}
		if obj, has := slip.GetArgsKeyValue(rest, slip.Symbol(":pad")); has {
			pad = byte(slip.ToOctet(obj).(slip.Octet))
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
