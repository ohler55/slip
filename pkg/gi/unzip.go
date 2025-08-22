// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"bytes"
	"compress/gzip"
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Unzip{Function: slip.Function{Name: "unzip", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "unzip",
			Args: []*slip.DocArg{
				{
					Name: "data",
					Type: "octets",
					Text: "The data to unzip. Must be coercible to _octets_.",
				},
			},
			Return: "octets,property-list",
			Text: `__unzip__ the _data_ and returned the uncompressed data along with a property list
of the header fields if there are not empty.`,
			Examples: []string{
				`(unzip (base64-decode "H4sICAAAAAAC/25hbWFpACrOz01VSEksSQQAAAD//wEAAP//HunC2QkAAAA="))`,
				`  => "some data", (:name "namai")`,
			},
		}, &Pkg)
}

// Unzip represents the unzip function.
type Unzip struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Unzip) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 12)
	data := []byte(slip.CoerceToOctets(args[0]).(slip.Octets))

	r, _ := gzip.NewReader(bytes.NewReader(data)) // can't fail
	// The gzip reader panic on error and does not return an error.
	buf, _ := io.ReadAll(r)
	var plist slip.List
	if 0 < len(r.Comment) {
		plist = append(plist, slip.Symbol(":comment"), slip.String(r.Comment))
	}
	if 0 < len(r.Extra) {
		plist = append(plist, slip.Symbol(":extra"), slip.Octets(r.Extra))
	}
	if 0 < len(r.Name) {
		plist = append(plist, slip.Symbol(":name"), slip.String(r.Name))
	}
	if r.OS != 255 {
		plist = append(plist, slip.Symbol(":os"), slip.Octet(r.OS))
	}
	if !r.ModTime.IsZero() {
		plist = append(plist, slip.Symbol(":mod-time"), slip.Time(r.ModTime.UTC()))
	}
	return slip.Values{slip.Octets(buf), plist}
}
