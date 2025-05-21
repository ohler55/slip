// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"bytes"
	"crypto/cipher"
	"crypto/rand"
	"io"
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := EncryptFile{Function: slip.Function{Name: "encrypt-file", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "encrypt-file",
			Args: []*slip.DocArg{
				{
					Name: "input-file",
					Type: "string",
					Text: "A file path to the file to encrypt.",
				},
				{
					Name: "output-file",
					Type: "string",
					Text: "A file path to the location to write the encrypted file.",
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
				{
					Name: "perm",
					Type: "fixnum",
					Text: "The file permissions for the _output-file_ if it is created.",
				},
			},
			Return: "nil",
			Text: `__encrypt-file__ reads the _input-file_, encrypts the content and then writes
the encrypted contents to _output-file_. Note that content that is less than a multiple of the
cipher block size will be padded with _:pad_ octets. If _key_ is not a supported block size it
is hashed before being used. If the _output-file_ can be the same as the _input-file_ the _input-file_
is overwritten.`,
			Examples: []string{
				`(encrypt-file "my-file.lisp" "my-file.enc" 'quux :cipher :des)`,
			},
		}, &Pkg)
}

// EncryptFile represents the encrypt-file function.
type EncryptFile struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *EncryptFile) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 3, 11)
	outPath := string(slip.CoerceToString(args[1]).(slip.String))
	data, err := os.ReadFile(string(slip.CoerceToString(args[0]).(slip.String)))
	if err != nil {
		panic(err)
	}
	non, pad, block, bsize := extractEncryptArgs(args[2:])
	perm := os.FileMode(0666)
	if 3 < len(args) {
		if obj, has := slip.GetArgsKeyValue(args[3:], slip.Symbol(":perm")); has {
			if num, ok := obj.(slip.Fixnum); ok {
				perm = os.FileMode(num)
			}
		}
	}
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

	if err = os.WriteFile(outPath, buf, perm); err != nil {
		panic(err)
	}
	return nil
}
