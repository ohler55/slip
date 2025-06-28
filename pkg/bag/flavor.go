// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"io"
	"strings"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var flavor *flavors.Flavor

func defBag() *flavors.Flavor {
	Pkg.Initialize(nil)
	flavor = flavors.DefFlavor("bag-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A bag is flexible a container for data composed of t, nil, integer,
float, string, time, list, as well as the special bag::map and
:false. It can be parsed from JSON or SEN (ojg package) and be encoded
in the same way. It can also be converted to and from native LISP with
bag::map becoming an assoc list. The bag::false value is the only
non-native value that is retained since LISP does not differentiate between
nil and boolean false.`),
			},
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(":parse"),
				slip.Symbol(":read"),
				slip.Symbol(":set"),
			},
		},
		&Pkg,
	)
	flavor.DefMethod(":init", "", initCaller{})
	flavor.DefMethod(":set", "", setCaller{})
	flavor.DefMethod(":parse", "", parseCaller{})
	flavor.DefMethod(":read", "", readCaller{})
	flavor.DefMethod(":get", "", getCaller{})
	flavor.DefMethod(":has", "", hasCaller{})
	flavor.DefMethod(":remove", "", removeCaller{})
	flavor.DefMethod(":modify", "", modifyCaller{})
	flavor.DefMethod(":native", "", nativeCaller{})
	flavor.DefMethod(":write", "", writeCaller{})
	flavor.DefMethod(":walk", "", walkCaller{})

	return flavor
}

// Flavor returns the bag-flavor.
func Flavor() *flavors.Flavor {
	return flavor
}

type initCaller struct{}

func (caller initCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	switch len(args) {
	case 0:
		// ok
	case 2:
		key, _ := args[0].(slip.Symbol)
		switch {
		case strings.EqualFold(":set", string(key)):
			obj.Any = ObjectToBag(args[1])
		case strings.EqualFold(":parse", string(key)):
			so, ok := args[1].(slip.String)
			if !ok {
				slip.PanicType("bag :init :parse", args[1], "string")
			}
			obj.Any = sen.MustParse([]byte(so))
			if options.Converter != nil {
				obj.Any = options.Converter.Convert(obj.Any)
			}
		case strings.EqualFold(":read", string(key)):
			r, ok := args[1].(io.Reader)
			if !ok {
				slip.PanicType("bag :init :read", args[1], "input-stream")
			}
			obj.Any = sen.MustParseReader(r)
			if options.Converter != nil {
				obj.Any = options.Converter.Convert(obj.Any)
			}
		}
	default:
		slip.PanicMethodArgChoice(obj, ":init", len(args), "0 or 2")
	}
	return nil
}

func (caller initCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: "Sets the initial value when _make-instance_ is called.",
		Args: []*slip.DocArg{
			{Name: "&key"},
			{
				Name: ":set",
				Type: "object",
				Text: "Sets the contents with the LISP construct provided.",
			},
			{
				Name: ":parse",
				Type: "string",
				Text: "Parses a JSON or SEN string to forms the contents.",
			},
			{
				Name: ":read",
				Type: "string",
				Text: "Reads from an _input-stream_ and parses read JSON or SEN to forms the contents.",
			},
		},
	}
}

type setCaller struct{}

func (caller setCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	switch len(args) {
	case 1:
		setBag(obj, args[0], nil)
	case 2:
		setBag(obj, args[0], args[1])
	default:
		slip.PanicMethodArgChoice(obj, ":set", len(args), "1 or 2")
	}
	return obj
}

func (caller setCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set",
		Text: `Sets a _value_ at the location described by _path_.
If no _path_ is provided the entire contents of the bag is replaced.`,
		Args: []*slip.DocArg{
			{
				Name: "value",
				Type: "object",
				Text: "The _value_ to set in _bag_ according to the path.",
			},
			{Name: "&optional"},
			{
				Name: "path",
				Type: "string|bag-path",
				Text: `The path to the location in the bag to set the _value_.
The path must follow the JSONPath format.`,
			},
		},
		Return: "self",
	}
}

type parseCaller struct{}

func (caller parseCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	switch len(args) {
	case 1:
		parseBag(obj, args[0], nil)
	case 2:
		parseBag(obj, args[0], args[1])
	default:
		slip.PanicMethodArgChoice(obj, ":parse", len(args), "1 or 2")
	}
	return obj
}

func (caller parseCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":parse",
		Text: `Parses _string_ and sets the parsed value at the location described by _path_.
If no _path_ is provided the entire contents of the bag is replaced.`,
		Args: []*slip.DocArg{
			{
				Name: "string",
				Type: "string",
				Text: "The string to parse and set in the instance according to the _path_.",
			},
			{Name: "&optional"},
			{
				Name: "path",
				Type: "string|bag-path",
				Text: `The path to the location in the bag to set the parsed value.
The path must follow the JSONPath format.`,
			},
		},
		Return: "self",
	}
}

type readCaller struct{}

func (caller readCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	switch len(args) {
	case 1:
		readBag(obj, args[0], nil)
	case 2:
		readBag(obj, args[0], args[1])
	default:
		slip.PanicMethodArgChoice(obj, ":read", len(args), "1 or 2")
	}
	return obj
}

func (caller readCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":read",
		Text: `Reads _stream_ and sets the read and parsed value at the location
described by _path_. If no _path_ is provided the entire contents of the bag is replaced.`,
		Args: []*slip.DocArg{
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
		Return: "self",
	}
}

type getCaller struct{}

func (caller getCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	switch len(args) {
	case 0:
		value = getBag(obj, nil, false)
	case 1:
		value = getBag(obj, args[0], false)
	case 2:
		value = getBag(obj, args[0], args[1] != nil)
	default:
		slip.PanicMethodArgCount(obj, ":get", len(args), 0, 2)
	}
	return
}

func (caller getCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":get",
		Text: `Gets a _value_ at the location described by _path_.
If no _path_ is provided the entire contents of the bag is returned.`,
		Args: []*slip.DocArg{
			{Name: "&optional"},
			{
				Name: "path",
				Type: "string|bag-path",
				Text: `Path to the location in the bag to get the _value_ from. The path must follow
the JSONPath format.`,
			},
			{
				Name: "as-bag",
				Type: "boolean",
				Text: "If not nil then the returned value is a _bag_ otherwise a new LISP value is returned.",
			},
		},
		Return: "object",
	}
}

type hasCaller struct{}

func (caller hasCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) == 1 {
		value = hasBag(obj, args[0])
	} else {
		slip.PanicMethodArgChoice(obj, ":has", len(args), "1")
	}
	return
}

func (caller hasCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":has",
		Text: `Returns true if a value at the location described by _path_ exists.`,
		Args: []*slip.DocArg{
			{
				Name: "path",
				Type: "string|bag-path",
				Text: `Path to the location in the bag to get the value from. The path must follow
the JSONPath format.`,
			},
		},
		Return: "boolean",
	}
}

type removeCaller struct{}

func (caller removeCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) == 1 {
		removeBag(obj, args[0])
	} else {
		slip.PanicMethodArgChoice(obj, ":remove", len(args), "1")
	}
	return obj
}

func (caller removeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":remove",
		Text: `Removes the value at the location described by _path_.
If no _path_ is provided the entire contents of the bag is removed and the bag value set to _nil_.`,
		Args: []*slip.DocArg{
			{Name: "&optional"},
			{
				Name: "path",
				Type: "string|bag-path",
				Text: `Path to the location in the bag to remove. The path must follow the JSONPath format.`,
			},
		},
		Return: "self",
	}
}

type modifyCaller struct{}

func (caller modifyCaller) Call(s *slip.Scope, args slip.List, depth int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	modifyBag(s, obj, args, depth+1)
	return obj
}

func (caller modifyCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":modify",
		Text: `Modifies the value at the location described by _path_.
If no _path_ is provided the entire contents of the bag is modified.`,
		Args: []*slip.DocArg{
			{
				Name: "function",
				Type: "symbol|lambda",
				Text: `The function to call to modify the value at _path_.`,
			},
			{Name: "&optional"},
			{
				Name: "path",
				Type: "string|bag-path",
				Text: `Path to the location in the bag to modify.
The path must follow the JSONPath format.`,
			},
			{Name: "&key"},
			{
				Name: ":as-bag",
				Type: "string|bag-path",
				Text: `If set to true then the value provided to _function_ will be a bag
instance otherwise it will be a lisp construct.`,
			},
		},
		Return: "self",
	}
}

type nativeCaller struct{}

func (caller nativeCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.PanicMethodArgChoice(obj, ":native", len(args), "0")
	}
	return slip.SimpleObject(obj.Any)
}

func (caller nativeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":native",
		Text:   `Returns the bag contents as a native LISP form.`,
		Return: "object",
	}
}

type writeCaller struct{}

func (caller writeCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	return writeBag(obj, args)
}

func (caller writeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":write",
		Text: `Writes the instance to _*standard-output*_, a provided output _stream_,
or to a string that is returned. If _stream_ is _t_ then output is to _*standard-output*_. If
_stream_ is _nil_ then output is a returned string. Any other _stream_ value must be an output
stream which is where output is written to. Output can be either JSON or SEN format as defined
in the OjG package.`,
		Args: []*slip.DocArg{
			{Name: "&optional"},
			{
				Name: "destination",
				Type: "t|nil|output-stream",
				Text: `Output stream. Default: nil (return a string).`,
			},
			{Name: "&key"},
			{
				Name: ":pretty",
				Type: "string|bag-path",
				Text: `The value to use in place of the _*print-pretty*_ value. If _t_ then the
JSON or SEN output is indented according to the other keyword options.`,
			},
			{
				Name: ":depth",
				Type: "integer",
				Text: `The maximum number of nested elements on a line in the output. A value
of zero outputs a tight single line output.`,
				Default: slip.Fixnum(4),
			},
			{
				Name:    ":right-margin",
				Type:    "integer",
				Text:    `The value to use in place of the _*print-right-margin*_ value.`,
				Default: slip.Symbol("*print-right-margin*"),
			},
			{
				Name:    ":time-format",
				Type:    "string",
				Text:    `The value to use in place of the _*bag-time-format*_ value.`,
				Default: slip.Symbol("*bag-time-format*"),
			},
			{
				Name:    ":time-wrap",
				Type:    "string",
				Text:    `The value to use in place of the _*bag-time-wrap*_ value.`,
				Default: slip.Symbol("*bag-time-wrap*"),
			},
			{
				Name: ":json",
				Type: "boolean",
				Text: `If true the output is JSON formatted otherwise output is SEN format.`,
			},
			{
				Name: ":color",
				Type: "boolean",
				Text: `If true the output is colorized.`,
			},
		},
		Return: "nil|string",
	}
}

type walkCaller struct{}

func (caller walkCaller) Call(s *slip.Scope, args slip.List, depth int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	walkBag(s, obj, args, depth)
	return nil
}

func (caller walkCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":walk",
		Text: `Walks the values at the location described by _path_.`,
		Args: []*slip.DocArg{
			{
				Name: "function",
				Type: "symbol|lambda",
				Text: `The function to apply to each node in the instance matching the _path_.`,
			},
			{Name: "&optional"},
			{
				Name:    "path",
				Type:    "string|bag-path",
				Text:    `The path to the location in the bag to walk. The path must follow the JSONPath format.`,
				Default: slip.String(".."),
			},
			{
				Name: ":as-lisp",
				Type: "boolean",
				Text: `If not nil then the value to the _function_ is a LISP value otherwise a new _bag_.`,
			},
		},
		Return: "nil",
	}
}
