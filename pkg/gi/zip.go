// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"bytes"
	"compress/gzip"
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Zip{Function: slip.Function{Name: "zip", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "zip",
			Args: []*slip.DocArg{
				{
					Name: "data",
					Type: "octets",
					Text: "The data to zip. Must be coercible to _octets_.",
				},
				{Name: "&optional"},
				{
					Name: "level",
					Type: "fixnum|nil",
					Text: "The compression level from 0 (none) to 9 (best) or nil for the default.",
				},
				{Name: "&key"},
				{
					Name: "comment",
					Type: "string",
					Text: "A comment for the zip header.",
				},
				{
					Name: "extra",
					Type: "octets",
					Text: "Extra for the zip header.",
				},
				{
					Name: "mod-time",
					Type: "time",
					Text: "Mod time for the zip header.",
				},
				{
					Name: "name",
					Type: "octets",
					Text: "Name for the zip header.",
				},
				{
					Name: "os",
					Type: "octet",
					Text: "OS for the zip header.",
				},
			},
			Return: "octets",
			Text: `__zip__ the _data_ using the
that data that is less than a multiple of the cipher block size will be padded with _:pad_ octets.
If _key_ is not a supported block size it is hashed before being used.`,
			Examples: []string{
				`(base64-encode (zip "some data")) => "H4sIAAAAAAAA/yrOz01VSEksSQQAAAD//wEAAP//HunC2QkAAAA="`,
			},
		}, &Pkg)
}

// Zip represents the zip function.
type Zip struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Zip) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 12)
	data := []byte(slip.CoerceToOctets(args[0]).(slip.Octets))
	level := gzip.DefaultCompression
	if 1 < len(args) {
		switch ta := args[1].(type) {
		case nil:
			// leave as the default
			args = args[1:]
		case slip.Fixnum:
			level = int(ta)
			args = args[1:]
		case slip.Symbol:
			// not a level
		default:
			slip.PanicType("level", ta, "fixnum", "nil")
		}
	}
	var buf bytes.Buffer
	w, err := gzip.NewWriterLevel(&buf, level)
	if err != nil {
		panic(err)
	}
	if 1 < len(args) {
		args = args[1:]
		if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":comment")); has {
			w.Comment = slip.MustBeString(val, ":comment")
		}
		if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":extra")); has {
			w.Extra = []byte(slip.CoerceToOctets(val).(slip.Octets))
		}
		if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":mod-time")); has {
			if st, ok := val.(slip.Time); ok {
				w.ModTime = time.Time(st).UTC()
			} else {
				slip.PanicType(":mod-time", val, "time")
			}
		}
		if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":name")); has {
			w.Name = slip.MustBeString(val, ":name")
		}
		if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":os")); has {
			w.OS = byte(slip.ToOctet(val).(slip.Octet))
		}
	}
	if _, err = w.Write(data); err != nil {
		panic(err)
	}
	_ = w.Flush()
	_ = w.Close()

	return slip.Octets(buf.Bytes())
}
