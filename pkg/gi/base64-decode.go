// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"encoding/base64"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Base64Decode{Function: slip.Function{Name: "base64-decode", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "base64-decode",
			Args: []*slip.DocArg{
				{
					Name: "source",
					Type: "string",
					Text: "The string to decoded.",
				},
			},
			Return: "string",
			Text:   `__base64-decode__ decodes a base64 (RFC 4648) encoded string into a string.`,
			Examples: []string{
				`(base64-decode "c2FtcGxl") => "sample"`,
			},
		}, &Pkg)
}

// Base64Decode represents the base64-decode function.
type Base64Decode struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Base64Decode) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	source := string(slip.CoerceToOctets(args[0]).(slip.Octets))

	decoded, err := base64.StdEncoding.DecodeString(source)
	if err != nil {
		panic(err)
	}
	return slip.String(decoded)
}
