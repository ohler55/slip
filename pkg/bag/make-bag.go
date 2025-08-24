// Copyright (c) 2024, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Make{Function: slip.Function{Name: "make-bag", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-bag",
			Args: []*slip.DocArg{
				{
					Name: "value",
					Type: "object",
					Text: "Either a string, octets or other object that can be converted to a bag",
				},
			},
			Return: "bag",
			Text: `__make-bag__ makes a new _bag_ by either parsing a _string_ as a
JSON or SEN document or converting an assoc to a map, a list to an array, and number
to an int64 or float64 in go terms.`,
			Examples: []string{
				`(make-bag "{a:7}") => #<bag-flavor 12345> ;; content is now {a:7}`,
			},
		}, &Pkg)
}

// Make represents the make-bag function.
type Make struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Make) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	self := flavor.MakeInstance().(*flavors.Instance)
	self.Init(s, slip.List{}, depth)

	switch ta := args[0].(type) {
	case slip.Octets:
		self.Any = sen.MustParse([]byte(ta))
		if options.Converter != nil {
			self.Any = options.Converter.Convert(self.Any)
		}
	case slip.String:
		self.Any = sen.MustParse([]byte(ta))
		if options.Converter != nil {
			self.Any = options.Converter.Convert(self.Any)
		}
	default:
		self.Any = ObjectToBag(s, args[0], depth)
	}
	return self
}
