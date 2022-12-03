// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"fmt"
	"strings"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var flavor *flavors.Flavor

func init() {
	flavor = flavors.DefFlavor("bag-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":set"),
				slip.Symbol(":parse"),
				slip.Symbol(":init-keywords"),
			},
			slip.List{
				slip.String(`A bag is flexible a container for data composed of t, nil, integer,
float, string, time, list, as well as the special bag::map and
:false. It can be parsed from JSON or SEN (ojg package) and be encoded
in the same way. It can also be converted to and from native LISP with
bag::map becoming an assoc list. The bag::false value is the only
non-native value that is retained since LSIP does not differentiate between
nil and boolean false.`),
				slip.Symbol(":documentation")},
		},
	)
	flavor.DefMethod(":init", "", initCaller(true))
	flavor.DefMethod(":set", "", setCaller(true))
	flavor.DefMethod(":parse", "", parseCaller(true))
	flavor.DefMethod(":get", "", getCaller(true))
	flavor.DefMethod(":has", "", hasCaller(true))
	flavor.DefMethod(":native", "", nativeCaller(true))
	flavor.DefMethod(":write", "", writeCaller(true))
	flavor.DefMethod(":walk", "", walkCaller(true))
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
		key, _ := args[1].(slip.Symbol)
		switch {
		case strings.EqualFold(":set", string(key)):
			obj.Any = objectToBag(args[0])
		case strings.EqualFold(":parse", string(key)):
			so, ok := args[0].(slip.String)
			if !ok {
				slip.PanicType("bag :init :parse", args[0], "string")
			}
			obj.Any = sen.MustParse([]byte(so))
			if options.Converter != nil {
				obj.Any = options.Converter.Convert(obj.Any)
			}
		}
	default:
		panic(fmt.Sprintf("Method bag-flavor init method expects zero or two arguments but received %d.", len(args)))
	}
	return nil
}

type setCaller bool

func (caller setCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	switch len(args) {
	case 1:
		setBag(obj, args[0], nil)
	case 2:
		setBag(obj, args[1], args[0])
	default:
		panic(fmt.Sprintf("Method bag-flavor set method expects one or two arguments but received %d.", len(args)))
	}
	return obj
}

type parseCaller bool

func (caller parseCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	switch len(args) {
	case 1:
		parseBag(obj, args[0], nil)
	case 2:
		parseBag(obj, args[1], args[0])
	default:
		panic(fmt.Sprintf("Method bag-flavor parse method expects one or two arguments but received %d.", len(args)))
	}
	return obj
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
		value = getBag(obj, args[1], args[0] != nil)
	default:
		panic(fmt.Sprintf("Method bag-flavor get method expects one or two arguments but received %d.", len(args)))
	}
	return
}

type hasCaller bool

func (caller hasCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) == 1 {
		value = hasBag(obj, args[0])
	} else {
		panic(fmt.Sprintf("Method bag-flavor has method expects one arguments but received %d.", len(args)))
	}
	return
}

type nativeCaller bool

func (caller nativeCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		panic(fmt.Sprintf("Method bag-flavor native method expects no arguments but received %d.", len(args)))
	}
	return slip.SimpleObject(obj.Any)
}

type writeCaller bool

func (caller writeCaller) Call(s *slip.Scope, args slip.List, _ int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	return writeBag(obj, args, len(args)-1)
}

type walkCaller bool

func (caller walkCaller) Call(s *slip.Scope, args slip.List, depth int) (value slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	walkBag(s, obj, args, len(args)-1, depth)
	return nil
}
