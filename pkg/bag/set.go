// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"strings"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

// TBD set function

func set(obj *flavors.Instance, args slip.List) {
	var x jp.Expr
	switch path := args[1].(type) {
	case nil:
	case slip.String:
		x = jp.MustParseString(string(path))
	default:
		// TBD Also handle bag-path
		slip.PanicType("path", path, "string")
	}
	v := objectToBag(args[0])
	if x == nil {
		obj.Any = v
	} else {
		x.MustSet(obj.Any, v)
	}
}

func objectToBag(obj slip.Object) any {
	var v any
	switch val := obj.(type) {
	case nil:
		// leave v as nil
	case slip.Symbol:
		if strings.EqualFold("false", string(val)) {
			v = false
		} else {
			v = string(val)
		}
	case slip.List:
		// TBD could be a simple list or assoc
		//  reverse direction
	case *flavors.Instance:
		if val.Flavor != flavor {
			slip.PanicType("value", val, "nil", "t", ":false", "integer", "float", "string", "symbol", "gi::time",
				"list", "hash-table", "bag-instance")
		}
		v = val.Any
	default:
		v = val.Simplify()
	}
	return v
}
