// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Parse{Function: slip.Function{Name: "bag-parse", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-parse",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to set a parsed value in.",
				},
				{
					Name: "string",
					Type: "string",
					Text: "The _string_ or _octets_ to parse and set in _bag_ according to the path.",
				},
				{Name: "&optional"},
				{
					Name: "path",
					Type: "string|bag-path",
					Text: `The path to the location in the bag to set the parsed value.
The path must follow the JSONPath format.`,
				},
			},
			Return: "bag",
			Text: `__bag-parse__ parses _string_ and sets the parsed value at the location described by _path_.
If no _path_ is provided the entire contents of the bag is replaced.

This is the same as the _:parse_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(parseq bag (make-instance 'bag-flavor :parse "{a:7}"))`,
				`(bag-parse bag "{b:5}" "a") => #<bag-flavor 12345> ;; content is now {a:{b:5}}`,
			},
		}, &Pkg)
}

// Parse represents the parse function.
type Parse struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Parse) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 3)
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Type != flavor {
		slip.TypePanic(s, depth, "bag", args[0], "bag")
	}

	if 2 < len(args) {
		parseBag(s, obj, args[1], args[2], depth)
	} else {
		parseBag(s, obj, args[1], nil, depth)
	}
	return obj
}

func parseBag(s *slip.Scope, obj *flavors.Instance, value, path slip.Object, depth int) {
	var x jp.Expr
	switch p := path.(type) {
	case nil:
	case slip.String:
		x = jp.MustParseString(string(p))
	case Path:
		x = jp.Expr(p)
	default:
		slip.TypePanic(s, depth, "path", p, "string")
	}
	var v any
	switch tv := value.(type) {
	case slip.String:
		v = sen.MustParse([]byte(tv))
	case slip.Octets:
		v = sen.MustParse([]byte(tv))
	default:
		slip.TypePanic(s, depth, "string", value, "string", "octets")
	}
	if options.Converter != nil {
		v = options.Converter.Convert(v)
	}
	if x == nil {
		obj.Any = v
	} else {
		x.MustSet(obj.Any, v)
	}
}
