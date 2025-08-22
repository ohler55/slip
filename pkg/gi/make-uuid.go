// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeUUID{Function: slip.Function{Name: "make-uuid", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-uuid",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "value",
					Type: "string|list",
					Text: "Value used to create the UUID.",
				},
			},
			Return: "uuid",
			Text: `__make-uuid__ returns a UUID based on the _value_ provided. If the _value_
is a string it must be in the format of "6ef16994-701d-44d5-87ec-7ef3e2e5709b". If the value is
a list of two fixnums they will be used as a high and low 64 parts of a 128 bit UUID. If no
_value_ is provided a random UUID is created.`,
			Examples: []string{
				`(make-uuid) => #<uuid 6ef16994-701d-44d5-87ec-7ef3e2e5709b>`,
			},
		}, &Pkg)
}

// MakeUUID represents the make-uuid function.
type MakeUUID struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeUUID) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 1)
	var uuid UUID
	if len(args) == 0 {
		uuid = NewUUID()
	} else {
		switch ta := args[0].(type) {
		case slip.String:
			uuid = UUIDParse(string(ta))
		case slip.List:
			if len(ta) != 2 {
				slip.TypePanic(s, depth, "value", ta, "string", "list of 2 fixnums")
			}
			var (
				ok  bool
				num slip.Fixnum
			)
			if num, ok = ta[0].(slip.Fixnum); ok {
				uuid[0] = uint64(num)
				if num, ok = ta[1].(slip.Fixnum); ok {
					uuid[1] = uint64(num)
				}
			}
			if !ok {
				slip.TypePanic(s, depth, "value", ta, "string", "list of 2 fixnums")
			}
		default:
			slip.TypePanic(s, depth, "value", ta, "string", "list of 2 fixnums")
		}
	}
	return uuid
}
