// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"encoding/base64"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Base64Encode{Function: slip.Function{Name: "base64-encode", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "base64-encode",
			Args: []*slip.DocArg{
				{
					Name: "source",
					Type: "string",
					Text: "The string to encoded.",
				},
			},
			Return: "string",
			Text:   `__base64-encode__ encodes the bytes in a string as base64 (RFC 4648).`,
			Examples: []string{
				`(base64-encode "sample") => "c2FtcGxl"`,
			},
		}, &Pkg)
}

// Base64Encode represents the base64-encode function.
type Base64Encode struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Base64Encode) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	source := slip.MustBeString(args[0], "source")

	return slip.String(base64.StdEncoding.EncodeToString([]byte(source)))
}
