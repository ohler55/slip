// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

// InstanceSymbol is the symbol with a value of "instance".
const InstanceSymbol = slip.Symbol("instance")

func init() {
	slip.DefConstant(InstanceSymbol, InstanceSymbol,
		`An _instance_ of a _flavor_.`)
}

// Instance is an instance of a Flavor.
type Instance struct {
	InstanceBase
	Flavor *Flavor
	// Any is available to go methods.
	Any any
}

// IsA return true if the instance is of a flavor that inherits from the
// provided flavor.
func (obj *Instance) IsA(flavor *Flavor) bool {
	if obj.Flavor == flavor {
		return true
	}
	for _, f := range obj.Flavor.inherit {
		if flavor == f {
			return true
		}
	}
	return false
}

// Init the instance slots from the provided args list. If the scope is not
// nil then send :init is called.
func (obj *Instance) Init(scope *slip.Scope, args slip.List, depth int) {
	obj.Keep = true
	var plist slip.List
	keys := map[string]bool{}
	cf := obj.Flavor
	for i := 0; i < len(args); i++ {
		sym, ok := args[i].(slip.Symbol)
		if !ok || len(sym) < 2 || sym[0] != ':' {
			slip.PanicType("initialization keyword", args[i], "keyword")
		}
		key := strings.ToLower(string(sym))
		if key == ":self" {
			slip.NewPanic("initialization keyword 'self' is not initable.")
		}
		i++
		val := args[i]
		if len(cf.initable) == 0 || cf.initable[key] {
			vkey := key[1:]
			if _, has := cf.defaultVars[vkey]; has {
				obj.Let(slip.Symbol(vkey), val)
				continue
			}
		}
		keys[key] = true
		if cf.allowOtherKeys {
			plist = append(plist, sym, val)
		} else if _, has := cf.keywords[key]; has {
			plist = append(plist, sym, val)
		} else {
			slip.NewPanic("%s is not an init keyword for flavor %s.", sym, cf.name)
		}
	}
	for _, k := range cf.requiredKeywords {
		if !keys[k] {
			slip.NewPanic("Keyword %s missing from initialization list for flavor %s.", k, cf.name)
		}
	}
	if scope != nil {
		_ = obj.Receive(scope, ":init", slip.List{plist}, depth+1)
	}
}

const (
	bold         = "\x1b[1m"
	colorOff     = "\x1b[m"
	indentSpaces = "                                                                                "
)

// Describe the instance in detail.
func (obj *Instance) Describe(b []byte, indent, right int, ansi bool) []byte {
	b = append(b, indentSpaces[:indent]...)
	if ansi {
		b = append(b, bold...)
		b = obj.Append(b)
		b = append(b, colorOff...)
	} else {
		b = obj.Append(b)
	}
	b = append(b, ", an instance of flavor "...)
	if ansi {
		b = append(b, bold...)
		b = append(b, obj.Flavor.name...)
		b = append(b, colorOff...)
	} else {
		b = append(b, obj.Flavor.name...)
	}
	b = append(b, ",\n"...)

	keys := make([]string, 0, len(obj.Vars))
	for k := range obj.Vars {
		if k != "self" {
			keys = append(keys, k)
		}
	}
	if 0 < len(keys) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "  has instance variable values:\n"...)
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:indent+4]...)
			b = append(b, k...)
			b = append(b, ": "...)
			b = slip.ObjectAppend(b, obj.Vars[k])
			b = append(b, '\n')
		}
	}
	return b
}

// Length returns the length of the object.
func (obj *Instance) Length() (size int) {
	if 0 < len(obj.Methods[":length"]) {
		v := obj.Receive(nil, ":length", slip.List{}, 0)
		if num, ok := v.(slip.Fixnum); ok {
			size = int(num)
		}
	} else {
		switch tv := obj.Any.(type) {
		case string:
			size = len(tv)
		case []any:
			size = len(tv)
		case map[string]any:
			size = len(tv)
		}
	}
	return
}

// Class returns the flavor of the instance.
func (obj *Instance) Class() slip.Class {
	return obj.Flavor
}
