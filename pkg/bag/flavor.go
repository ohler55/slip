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
	flavor.DefMethod(":init", "", initCaller(true))
	flavor.DefMethod(":set", "", setCaller(true))
	flavor.DefMethod(":parse", "", parseCaller(true))
	flavor.DefMethod(":read", "", readCaller(true))
	flavor.DefMethod(":get", "", getCaller(true))
	flavor.DefMethod(":has", "", hasCaller(true))
	flavor.DefMethod(":remove", "", removeCaller(true))
	flavor.DefMethod(":modify", "", modifyCaller(true))
	flavor.DefMethod(":native", "", nativeCaller(true))
	flavor.DefMethod(":write", "", writeCaller(true))
	flavor.DefMethod(":walk", "", walkCaller(true))

	return flavor
}

// Flavor returns the bag-flavor.
func Flavor() *flavors.Flavor {
	return flavor
}

type initCaller bool

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

func (caller initCaller) Docs() string {
	return `__:init__ &key _set_ _parse_
   _:set_ sets the contents with the LISP construct provided.
   _:parse_ parses a JSON or SEN string to forms the contents.
   _:read_ reads from an _input-stream_ and parses read JSON or SEN to forms the contents.

Sets the initial value when _make-instance_ is called.
`
}

type setCaller bool

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

func (caller setCaller) Docs() string {
	return `__:set__ _value_ &optional _path_ => _self_
  _value_ The value to set in the instance according to the path.
  _path_ The path to the location in the bag to set the _value_.
The path must follow the JSONPath format.

Sets a _value_ at the location described by _path_.
If no _path_ is provided the entire contents of the bag is replaced.
`
}

type parseCaller bool

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

func (caller parseCaller) Docs() string {
	return `__:parse__ _string_ &optional _path_ => _self_
  _string_ The string to parse and set in the instance according to the _path_.
  _path_ The path to the location in the bag to set the parsed value.
The path must follow the JSONPath format.


Parses _string_ and sets the parsed value at the location described by _path_.
If no _path_ is provided the entire contents of the bag is replaced.
`
}

type readCaller bool

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

func (caller readCaller) Docs() string {
	return `__:read__ _string_ &optional _path_ => _self_
  _stream_ The _input-stream_ to read and set in the instance according to the _path_.
  _path_ The path to the location in the bag to set the readd value.
The path must follow the JSONPath format.


Read from _stream_ and sets the parsed value at the location described by _path_.
If no _path_ is provided the entire contents of the bag is replaced.
`
}

type getCaller bool

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

func (caller getCaller) Docs() string {
	return `__:get__ &optional _path_ _as-bag_ => _object_|_bag_
  _path_ to the location in the bag to get the _value_ from. The path must follow the JSONPath format.
  _as-bag_ if not nil then the returned value is a _bag_ otherwise a new LISP value is returned.


Gets a _value_ at the location described by _path_.
If no _path_ is provided the entire contents of the bag is returned.
.`
}

type hasCaller bool

func (caller hasCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) == 1 {
		value = hasBag(obj, args[0])
	} else {
		slip.PanicMethodArgChoice(obj, ":has", len(args), "1")
	}
	return
}

func (caller hasCaller) Docs() string {
	return `__:has__ _path_ => _boolean_
  _path_ to the location in the bag to get the value from. The path must follow the JSONPath format.


Returns true if a value at the location described by _path_ exists.
`
}

type removeCaller bool

func (caller removeCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) == 1 {
		removeBag(obj, args[0])
	} else {
		slip.PanicMethodArgChoice(obj, ":remove", len(args), "1")
	}
	return obj
}

func (caller removeCaller) Docs() string {
	return `__:remove__ _path_ => _object_
  _path_ to the location in the bag to remove. The path must follow the JSONPath format.


Returns the object itself.
`
}

type modifyCaller bool

func (caller modifyCaller) Call(s *slip.Scope, args slip.List, depth int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	modifyBag(s, obj, args, depth+1)
	return obj
}

func (caller modifyCaller) Docs() string {
	return `__:modify__ _function_ &optional _path_ &key _:as-bag_ => _object_
  _function_ to modify the value at _path_
  _path_ to the location in the bag to modify. The path must follow the JSONPath format.
  _:as-bag_ if true the _function_ expects a _bag_ otherwise it expects a lisp object.


Returns the object itself.
`
}

type nativeCaller bool

func (caller nativeCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.PanicMethodArgChoice(obj, ":native", len(args), "0")
	}
	return slip.SimpleObject(obj.Any)
}

func (caller nativeCaller) Docs() string {
	return `__:native__ => _object_

Returns the bag contents as a native LISP form.
`
}

type writeCaller bool

func (caller writeCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	return writeBag(obj, args)
}

func (caller writeCaller) Docs() string {
	return `__:write__ &optional _stream_ &key _pretty_ _depth_ _right-margin_ _time-format_ _time-wrap_ _json_ _color_
  _stream_ an output-stream. Default: nil (return a string).
  _:pretty_ The value to use in place of the _*print-pretty*_ value. If _t_ then the
JSON or SEN output is indented according to the other keyword options.
  _:depth_ The maximum number of nested elements on a line in the output. A value
of zero outputs a tight single line output. Default: 4.
  _:right-margin_ The value to use in place of the _*print-right-margin*_ value.
  _:time-format_ The value to use in place of the _*bag-time-format*_ value.
  _:time-wrap_ The value to use in place of the _*bag-time-wrap*_ value.
  _:json_ If true the output is JSON formatted otherwise output is SEN format.
  _:color_ If true the output is colorized.


Writes the instance to _*standard-output*_, a provided output _stream_,
or to a string that is returned. If _stream_ is _t_ then output is to _*standard-output*_. If
_stream_ is _nil_ then output is a returned string. Any other _stream_ value must be an output
stream which is where output is written to. Output can be either JSON or SEN format as defined
in the OjG package.
`
}

type walkCaller bool

func (caller walkCaller) Call(s *slip.Scope, args slip.List, depth int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	walkBag(s, obj, args, depth)
	return nil
}

func (caller walkCaller) Docs() string {
	return `__:walk__ _function_ &optional _path_ _as-lisp_
  _function_ The function to apply to each node in the instance matching the _path_.
  _path_ The path to the location in the bag to walk. The path must follow the JSONPath format. Default: ".."
  _as-lisp_ If not nil then the value to the _function_ is a LISP value otherwise a new _bag_.


Walks the values at the location described by _path_.
`
}
