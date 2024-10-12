// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"io"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Read{Function: slip.Function{Name: "bag-read", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-read",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to set a read and parsed value in.",
				},
				{
					Name: "stream",
					Type: "input-stream",
					Text: "The _stream_ to read, parse, and set in _bag_ according to the path.",
				},
				{Name: "&optional"},
				{
					Name: "path",
					Type: "string|bag-path",
					Text: `The path to the location in the bag to set the read value.
The path must follow the JSONPath format.`,
				},
			},
			Return: "bag",
			Text: `__bag-read__ reads _stream_ and sets the read and parsed value at the location
described by _path_. If no _path_ is provided the entire contents of the bag is replaced.

This is the same as the _:read_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :read "{a:7}"))`,
				`(bag-read bag (make-string-input-stream "{b:5}") "a") => #<bag-flavor 12345> ;; content is {a:{b:5}}`,
			},
		}, &Pkg)
}

// Read represents the read function.
type Read struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Read) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 3)
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Type != flavor {
		slip.PanicType("bag", args[0], "bag")
	}
	if 2 < len(args) {
		readBag(obj, args[1], args[2])
	} else {
		readBag(obj, args[1], nil)
	}
	return obj
}

func readBag(obj *flavors.Instance, value, path slip.Object) {
	var x jp.Expr
	switch p := path.(type) {
	case nil:
	case slip.String:
		x = jp.MustParseString(string(p))
	case Path:
		x = jp.Expr(p)
	default:
		slip.PanicType("path", p, "string")
	}
	r, ok := value.(io.Reader)
	if !ok {
		slip.PanicType("stream", value, "input-stream")
	}
	v := sen.MustParseReader(r)
	if options.Converter != nil {
		v = options.Converter.Convert(v)
	}
	if x == nil {
		obj.Any = v
	} else {
		x.MustSet(obj.Any, v)
	}
}
