// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"io"
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

// FlavorSymbol is the symbol with a value of "flavor".
const FlavorSymbol = slip.Symbol("flavor")

var allFlavors = map[string]*Flavor{vanilla.name: &vanilla}

// Flavor of Objects.
type Flavor struct {
	name             string
	docs             string
	inherit          []*Flavor
	defaultVars      map[string]slip.Object
	keywords         map[string]slip.Object
	methods          map[string]*slip.Method
	included         []string
	required         []string
	requiredMethods  []string
	requiredVars     []string
	requiredKeywords []string
	varNames         []string // for DefMethod, requiredVars and defaultVars combined
	varDocs          map[string]string
	initable         map[string]bool
	defaultHandler   slip.Caller
	abstract         bool
	noVanilla        bool
	allowOtherKeys   bool
	pkg              *slip.Package
	precedence       []slip.Symbol
	Final            bool
	GoMakeOnly       bool
}

// Find the named flavor.
func Find(name string) (f *Flavor) {
	if f = allFlavors[name]; f == nil {
		f = allFlavors[strings.ToLower(name)]
	}
	return
}

// All returns a list of all defined flavors.
func All() (all []*Flavor) {
	all = make([]*Flavor, 0, len(allFlavors))
	for _, f := range allFlavors {
		all = append(all, f)
	}
	return
}

// Pkg returns the package the flavor was defined in.
func (obj *Flavor) Pkg() *slip.Package {
	return obj.pkg
}

// DefMethod adds a method to the Flavor.
func (obj *Flavor) DefMethod(name string, daemon string, caller slip.Caller) {
	DefMethod(obj, obj.methods, name, daemon, caller)
}

// DefMethod adds a method to the class or flavor.
func DefMethod(obj slip.Class, mm map[string]*slip.Method, name, daemon string, caller slip.Caller) {
	name = strings.ToLower(name)
	var (
		addMethod bool
		addCombo  bool
	)
	m := mm[name]
	if m == nil {
		m = &slip.Method{Name: name, Doc: &slip.FuncDoc{Name: name, Kind: slip.MethodSymbol}}
		addMethod = true
	}
	// Override existing method documentation if provided by the caller.
	switch tc := caller.(type) {
	case slip.HasFuncDocs:
		if fd := tc.FuncDocs(); fd != nil && 0 < len(fd.Text) {
			fd.Kind = slip.MethodSymbol
			if !addMethod && name != ":init" {
				m.CompareArgs(fd)
			}
			m.Doc = fd
		}
	case HasDocs:
		if text := tc.Docs(); 0 < len(text) {
			m.Doc = &slip.FuncDoc{Name: name, Text: text, Kind: slip.MethodSymbol}
		}
	}
	// If there is a combination for this flavor it will be the first on the
	// list.
	var c *slip.Combination
	if 0 < len(m.Combinations) && m.Combinations[0].From == obj {
		c = m.Combinations[0]
	} else {
		addCombo = true
		c = &slip.Combination{From: obj}
	}
	daemon = strings.ToLower(daemon)
	switch daemon {
	case ":primary", "":
		c.Primary = caller
	case ":before":
		c.Before = caller
	case ":after":
		c.After = caller
	case ":whopper", ":wrapper":
		c.Wrap = caller
	default:
		slip.PanicMethod(obj, slip.Symbol(daemon), slip.Symbol(name), "")
	}
	if addMethod {
		mm[name] = m
	}
	if addCombo {
		m.Combinations = append([]*slip.Combination{c}, m.Combinations...)
	}
	if addCombo {
		if flavor, ok := obj.(*Flavor); ok {
			// If there are supers that inherit from this flavor then insert
			// the new method into the method combinations.
			fname := obj.Name()
			for _, f := range allFlavors {
				if f.inheritsFlavor(fname) {
					f.insertMethod(flavor, m, c)
				}
			}
		}
	}
}

func (obj *Flavor) insertMethod(super *Flavor, method *slip.Method, combo *slip.Combination) {
	m := obj.methods[method.Name]
	if m == nil {
		m = &slip.Method{
			Name:         method.Name,
			Doc:          method.Doc,
			Combinations: []*slip.Combination{combo},
		}
		obj.methods[method.Name] = m
		return
	}
	var pos int
	if pos < len(m.Combinations) && m.Combinations[pos].From == obj {
		pos++
	}
	for _, f := range obj.inherit {
		if len(m.Combinations) <= pos || m.Combinations[pos].From == super {
			break
		}
		if m.Combinations[pos].From == f {
			pos++
		}
	}
	m.Combinations = append(append(m.Combinations[:pos], combo), m.Combinations[pos:]...)
}

// String representation of the Object.
func (obj *Flavor) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Flavor) Append(b []byte) []byte {
	b = append(b, "#<flavor "...)
	b = append(b, obj.name...)
	return append(b, '>')
}

// Metaclass returns the symbol flavor.
func (obj *Flavor) Metaclass() slip.Symbol {
	return slip.Symbol("flavor")
}

// Simplify by returning the string representation of the flavor.
func (obj *Flavor) Simplify() any {
	flist := make([]any, 0, len(obj.inherit))
	for _, f := range obj.inherit {
		flist = append(flist, f.name)
	}
	vars := map[string]any{}
	for k, o := range obj.defaultVars {
		vars[k] = slip.Simplify(o)
	}
	keywords := map[string]any{}
	for k, o := range obj.keywords {
		keywords[k] = slip.Simplify(o)
	}
	methods := make([]any, 0, len(obj.methods))
	names := make([]string, 0, len(obj.methods))

	for name := range obj.methods {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		methods = append(methods, obj.methods[name].Simplify())
	}
	initable := map[string]any{}
	for k := range obj.initable {
		initable[k] = true
	}
	_, isDefHand := obj.defaultHandler.(defHand)
	return map[string]any{
		"name":             obj.name,
		"package":          obj.pkg.Name,
		"docs":             obj.docs,
		"inherit":          flist,
		"defaultVars":      vars,
		"methods":          methods,
		"keywords":         keywords,
		"included":         obj.simplifyStringArray(obj.included),
		"required":         obj.simplifyStringArray(obj.required),
		"requiredMethods":  obj.simplifyStringArray(obj.requiredMethods),
		"requiredVars":     obj.simplifyStringArray(obj.requiredVars),
		"requiredKeywords": obj.simplifyStringArray(obj.requiredKeywords),
		"initable":         initable,
		"defaultHandler":   !isDefHand,
		"abstract":         obj.abstract,
		"allowOtherKeys":   obj.allowOtherKeys,
	}
}

func (obj *Flavor) simplifyStringArray(sa []string) (simple []any) {
	for _, s := range sa {
		simple = append(simple, s)
	}
	return
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Flavor) Equal(other slip.Object) (eq bool) {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Flavor) Hierarchy() []slip.Symbol {
	return []slip.Symbol{FlavorSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj *Flavor) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

func (obj *Flavor) inheritsFlavor(name string) bool {
	for _, f2 := range obj.inherit {
		if f2.name == name {
			return true
		}
	}
	return false
}

// Inherits returns true if this Class inherits from a specified Class.
func (obj *Flavor) Inherits(sc slip.Class) bool {
	for _, f2 := range obj.inherit {
		if f2 == sc {
			return true
		}
	}
	return false
}

func (obj *Flavor) inheritFlavor(cf *Flavor) {
	for _, f2 := range obj.inherit {
		if f2 == cf {
			return
		}
	}
	obj.inherit = append(obj.inherit, cf)
	if cf.allowOtherKeys {
		obj.allowOtherKeys = true
	}
	for k, v := range cf.defaultVars {
		if _, has := obj.defaultVars[k]; !has {
			obj.defaultVars[k] = v
		}
	}
	for k, v := range cf.keywords {
		if _, has := obj.keywords[k]; !has {
			obj.keywords[k] = v
		}
	}
	slip.InheritMethods(obj.methods, cf.methods)
	for _, f2 := range cf.inherit {
		if &vanilla != f2 {
			obj.inheritFlavor(f2)
		}
	}
}

// MakeInstance creates a new instance but does not call the :init method.
func (obj *Flavor) MakeInstance() slip.Instance {
	if obj.abstract || obj.GoMakeOnly {
		slip.NewPanic("Can not create an instance of flavor %s.", obj.name)
	}
	inst := Instance{Type: obj}
	inst.Vars = map[string]slip.Object{}
	inst.SetSynchronized(true)
	for k, v := range obj.defaultVars {
		inst.Vars[k] = v
	}
	inst.Vars["self"] = &inst

	return &inst
}

// Describe the flavor in detail.
func (obj *Flavor) Describe(b []byte, indent, right int, ansi bool) []byte {
	b = append(b, indentSpaces[:indent]...)
	if ansi {
		b = append(b, bold...)
		b = append(b, obj.name...)
		b = append(b, colorOff...)
	} else {
		b = append(b, obj.name...)
	}
	if obj.abstract {
		b = append(b, " is an abstract flavor:\n"...)
	} else {
		b = append(b, " is a flavor:\n"...)
	}
	i2 := indent + 2
	i3 := indent + 4
	i4 := indent + 6
	if 0 < len(obj.docs) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Documentation:\n"...)
		b = slip.AppendDoc(b, obj.docs, i3, right, ansi)
		b = append(b, '\n')
	}
	if 0 < len(obj.inherit) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Inherits:"...)
		for _, f := range obj.inherit {
			b = append(b, ' ')
			b = append(b, f.name...)
		}
		b = append(b, '\n')
	}
	if 0 < len(obj.defaultVars) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Variables:\n"...)
		var keys []string
		for k := range obj.defaultVars {
			if k == "self" {
				continue
			}
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, " = "...)
			b = slip.Append(b, obj.defaultVars[k])
			if obj.initable[":"+k] {
				b = append(b, " (initable)"...)
			}
			b = append(b, '\n')
			if desc := obj.varDocs[k]; 0 < len(desc) {
				b = slip.AppendDoc(b, desc, i4, right, ansi)
				b = append(b, '\n')
			}
		}
	}
	if 0 < len(obj.keywords) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Keywords with default values:\n"...)
		var keys []string
		for k := range obj.keywords {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, " = "...)
			b = slip.Append(b, obj.keywords[k])
			b = append(b, '\n')
		}
	}
	if obj.allowOtherKeys {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Allow Other Keywords: true\n"...)
	}
	if 0 < len(obj.methods) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Methods:\n"...)
		var keys []string
		for k := range obj.methods {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, '\n')
		}
	}
	b = obj.describeStrings(b, "Included Flavors", obj.included, indent, right)
	b = obj.describeStrings(b, "Required Flavors", obj.required, indent, right)
	b = obj.describeStrings(b, "Required Variable", obj.requiredVars, indent, right)
	b = obj.describeStrings(b, "Required Methods", obj.requiredMethods, indent, right)
	b = obj.describeStrings(b, "Required Keywords", obj.requiredKeywords, indent, right)

	return b
}

func (obj *Flavor) describeStrings(b []byte, label string, list []string, indent, right int) []byte {
	if 0 < len(list) {
		b = append(b, indentSpaces[:indent+2]...)
		b = append(b, label...)
		b = append(b, ":\n"...)
		for _, s := range list {
			b = append(b, indentSpaces[:indent+4]...)
			b = append(b, s...)
			b = append(b, '\n')
		}
	}
	return b
}

// Receive a method invocation from the send function. Not intended to be
// call by any code other than the send function but is public to allow it
// to be over-ridden.
func (obj *Flavor) Receive(_ *slip.Scope, message string, args slip.List, depth int) (result slip.Object) {
	var lo bool
top:
	switch message {
	case ":name":
		result = slip.String(obj.name)
	case ":describe":
		w := slip.CurrentPackage.JustGet("*standard-output*").(io.Writer)
		if 0 < len(args) {
			var ok bool
			if w, ok = args[0].(io.Writer); !ok {
				slip.PanicType("describe output-stream", args[0], "output-stream")
			}
		}
		ansi := slip.CurrentPackage.JustGet("*print-ansi*") != nil
		right := int(slip.CurrentPackage.JustGet("*print-right-margin*").(slip.Fixnum))
		_, _ = w.Write(obj.Describe(nil, 0, right, ansi))
		result = slip.Novalue
	case ":which-operations":
		result = slip.List{
			slip.Symbol(":describe"),
			slip.Symbol(":document"),
			slip.Symbol(":inspect"),
			slip.Symbol(":name"),
			slip.Symbol(":which-operations"),
		}
	case ":inspect":
		cf := allFlavors["bag-flavor"]
		inst := cf.MakeInstance().(*Instance)
		inst.Any = obj.Simplify()
		result = inst
	case ":document":
		if len(args) != 2 {
			slip.NewPanic(":document expects two arguments.")
		}
		name := slip.MustBeString(args[0], "name")
		desc := slip.MustBeString(args[1], "description")
		obj.varDocs[name] = desc
	default:
		if lo {
			slip.PanicUnboundSlot(obj, slip.Symbol(message), "%s is not a valid method for a Flavor.", message)
		}
		message = strings.ToLower(message)
		lo = true
		goto top
	}
	return
}

// Name of the class.
func (obj *Flavor) Name() string {
	return obj.name
}

// Documentation of the class.
func (obj *Flavor) Documentation() string {
	return obj.docs
}

// SetDocumentation of the class.
func (obj *Flavor) SetDocumentation(doc string) {
	obj.docs = doc
}

// Document a variable or method.
func (obj *Flavor) Document(name, desc string) {
	obj.varDocs[name] = desc
}

// GetMethod returns the method if it exists.
func (obj *Flavor) GetMethod(name string) *slip.Method {
	return obj.methods[name]
}

// MethodNames returns a sorted list of the methods of the class.
func (obj *Flavor) MethodNames() slip.List {
	names := make([]string, 0, len(obj.methods))
	for k := range obj.methods {
		names = append(names, k)
	}
	sort.Strings(names)
	methods := make(slip.List, len(names))
	for i, name := range names {
		methods[i] = slip.Symbol(name)
	}
	return methods
}

// DefList returns a list that can be evaluated to create the class or nil if
// the class is a built in class.
func (obj *Flavor) DefList() slip.List {
	keys := make([]string, 0, len(obj.defaultVars)-1)
	for k, v := range obj.defaultVars {
		if k != "self" && !obj.inheritedVar(k, v) {
			keys = append(keys, k)
		}
	}
	sort.Strings(keys)
	var (
		gets slip.List
		sets slip.List
	)
	ivs := make(slip.List, len(keys))
	for i, k := range keys {
		ksym := slip.Symbol(k)
		if v := obj.defaultVars[k]; v != nil {
			ivs[i] = slip.List{ksym, v}
		} else {
			ivs[i] = ksym
		}
		if _, has := obj.methods[":"+k]; has {
			gets = append(gets, ksym)
		}
		if _, has := obj.methods[":set-"+k]; has {
			sets = append(sets, ksym)
		}
	}
	var inh slip.List
	for _, f := range obj.inherit {
		if f.name == "vanilla-flavor" {
			continue
		}
		inh = append(inh, slip.Symbol(f.name))
	}
	df := slip.List{
		slip.Symbol("defflavor"),
		slip.Symbol(obj.name),
		ivs,
		inh,
	}
	if 0 < len(obj.initable) {
		if len(obj.initable) == len(keys) {
			df = append(df, slip.Symbol(":inittable-instance-variables"))
		} else {
			var iiv slip.List
			iiv = append(iiv, slip.Symbol(":inittable-instance-variables"))
			for k, v := range obj.initable {
				if v {
					iiv = append(iiv, slip.Symbol(k[1:]))
				}
			}
			df = append(df, iiv)
		}
	}
	if 0 < len(gets) {
		if len(gets) == len(keys) {
			df = append(df, slip.Symbol(":gettable-instance-variables"))
		} else {
			df = append(df, append(slip.List{slip.Symbol(":gettable-instance-variables")}, gets...))
		}
	}
	if 0 < len(sets) {
		if len(sets) == len(keys) {
			df = append(df, slip.Symbol(":settable-instance-variables"))
		} else {
			df = append(df, append(slip.List{slip.Symbol(":settable-instance-variables")}, sets...))
		}
	}
	if 0 < len(obj.keywords) {
		kws := make([]string, 0, len(obj.keywords))
		for k := range obj.keywords {
			kws = append(kws, k)
		}
		sort.Strings(kws)
		var ko slip.List
		ko = append(ko, slip.Symbol(":default-init-plist"))
		if obj.allowOtherKeys {
			ko = append(ko, slip.List{slip.Symbol(":allow-other-keys"), slip.True})
		}
		for _, k := range kws {
			ko = append(ko, slip.List{slip.Symbol(k), obj.keywords[k]})
		}
		df = append(df, ko)
	}
	if obj.abstract {
		df = append(df, slip.Symbol(":abstract-flavor"))
	}
	if obj.noVanilla {
		df = append(df, slip.Symbol(":no-vanilla-flavor"))
	}
	df = appendStringSliceOption(df, ":required-instance-variables", obj.requiredVars)
	df = appendStringSliceOption(df, ":required-methods", obj.requiredMethods)
	df = appendStringSliceOption(df, ":required-flavors", obj.required)
	df = appendStringSliceOption(df, ":included-flavors", obj.included)
	df = appendStringSliceOption(df, ":required-init-keywords", obj.requiredKeywords)
	if lam, ok := obj.defaultHandler.(*slip.Lambda); ok {
		if lam.Doc.Name == "lambda" {
			df = append(df, slip.List{slip.Symbol(":default-handler"), lam.DefList()})
		} else {
			df = append(df, slip.List{slip.Symbol(":default-handler"), slip.Symbol(lam.Doc.Name)})
		}
	}
	if 0 < len(obj.docs) {
		df = append(df, slip.List{slip.Symbol(":documentation"), slip.String(obj.docs)})
	}
	return df
}

func (obj *Flavor) inheritedVar(k string, v slip.Object) bool {
	for _, f := range obj.inherit {
		if iv, has := f.defaultVars[k]; has {
			return v == iv
		}
	}
	return false
}

func appendStringSliceOption(df slip.List, name string, ss []string) slip.List {
	if 0 < len(ss) {
		opt := make(slip.List, len(ss)+1)
		opt[0] = slip.Symbol(name)
		for i, str := range ss {
			opt[i+1] = slip.Symbol(str)
		}
		df = append(df, opt)
	}
	return df
}

// DefMethodList returns a list that can be evaluated to define a method on
// the class or nil if no method is defined by the class.
func (obj *Flavor) DefMethodList(method, daemon string, inherited bool) (dml slip.List) {
	method = strings.ToLower(method)
	daemon = strings.ToLower(daemon)
	var lam *slip.Lambda
	if m := obj.GetMethod(method); m != nil {
		for _, c := range m.Combinations {
			if obj == c.From || inherited {
				switch daemon {
				case ":primary", "":
					lam, _ = c.Primary.(*slip.Lambda)
				case ":before":
					lam, _ = c.Before.(*slip.Lambda)
				case ":after":
					lam, _ = c.After.(*slip.Lambda)
				case ":whopper":
					lam, _ = c.Wrap.(*slip.Lambda)
				}
			}
			if lam != nil {
				dml = slip.List{
					slip.Symbol("defmethod"),
					slip.List{slip.Symbol(obj.name), slip.Symbol(daemon), slip.Symbol(method)},
				}
				dml = append(dml, lam.DefList()[1:]...)
				break
			}
		}
	}
	return
}
