// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"bytes"
	"crypto/cipher"
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DecryptFile{Function: slip.Function{Name: "decrypt-file", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "decrypt-file",
			Args: []*slip.DocArg{
				{
					Name: "input-file",
					Type: "string",
					Text: "A file path to the file to decrypt.",
				},
				{
					Name: "output-file",
					Type: "string",
					Text: "A file path to the location to write the decrypted file.",
				},
				{
					Name: "key",
					Type: "string",
					Text: "The key for the decryption. Must be coercible to _octets_.",
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
				{
					Name: "perm",
					Type: "fixnum",
					Text: "The file permissions for the _output-file_ if it is created.",
				},
			},
			Return: "octets",
			Text: `__decrypt-file__ reads the _input-file_, decrypts the content and then writes
the decrypted contents to _output-file_. If _key_ is not a supported block size it
is hashed before being used. If the _output-file_ can be the same as the _input-file_ the _input-file_
is overwritten.`,
			Examples: []string{
				`(decrypt-file "my-file.enc" "my-file.lisp" 'quux :cipher :des)`,
			},
		}, &Pkg)
}

// DecryptFile represents the decrypt-file function.
type DecryptFile struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DecryptFile) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 3, 9)
	outPath := string(slip.CoerceToString(args[1]).(slip.String))
	data, err := os.ReadFile(string(slip.CoerceToString(args[0]).(slip.String)))
	if err != nil {
		panic(err)
	}
	strip, block, bsize, trim := extractDencryptArgs(s, args[2:], depth)
	perm := os.FileMode(0666)
	if 3 < len(args) {
		if obj, has := slip.GetArgsKeyValue(args[3:], slip.Symbol(":perm")); has {
			if num, ok := obj.(slip.Fixnum); ok {
				perm = os.FileMode(num)
			}
		}
	}
	if len(data) < bsize {
		slip.ErrorPanic(s, depth, "decrypt-file data is too short")
	}
	non := data[:bsize]
	data = data[bsize:]
	mode := cipher.NewCBCDecrypter(block, non)
	mode.CryptBlocks(data, data)
	if trim {
		data = bytes.TrimRight(data, string([]byte{strip}))
	}
	if err = os.WriteFile(outPath, data, perm); err != nil {
		panic(err)
	}
	return nil
}
