// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"sort"
	"strings"
	"sync"

	"github.com/ohler55/slip"
)

// Aux encapsulates the auxiliary data for a generic function.
type Aux struct {
	docs   *slip.FuncDoc // var-symbol, &optional, &rest, &key &allow-other-keys
	reqCnt int
	// Keys to the cache and methods is a join of then types separated by '|'.
	cache         map[string]*slip.Method
	methods       map[string]*slip.Method
	defaultKey    string
	defaultCaller slip.Caller
	moo           sync.Mutex
}

// NewAux creates a new generic aux.
func NewAux(fd *slip.FuncDoc) *Aux {
	var (
		cnt int
		dk  []byte
	)
	for _, da := range fd.Args {
		if da.Name[0] == '&' {
			break
		}
		dk = append(dk, 't', '|')
		cnt++
	}
	return &Aux{
		cache:      map[string]*slip.Method{},
		methods:    map[string]*slip.Method{},
		docs:       fd,
		reqCnt:     cnt,
		defaultKey: string(dk[:len(dk)-2]),
	}
}

// Call the the function with the arguments provided.
func (aux *Aux) Call(gf slip.Object, s *slip.Scope, args slip.List, depth int) slip.Object {
	// All calls must match the gf-lambda-list so check here as much as
	// reasonable.
	if len(args) < aux.reqCnt {
		slip.NewPanic("generic-function %s requires at least %d arguments. Received %d.",
			aux.docs.Name, aux.reqCnt, len(args))
	}
	aux.moo.Lock()
	if aux.defaultCaller != nil {
		caller := aux.defaultCaller
		aux.moo.Unlock()
		return caller.Call(s, args, depth)
	}
	// Any further argument checking gets tricky as optinal could be keywords
	// depending on then method's forms.
	key := buildSpecKey(args[:aux.reqCnt])
	meth := aux.cache[key]
	if meth == nil {
		if meth = aux.buildCacheMeth(args); meth != nil {
			aux.cache[key] = meth
		}
	}
	aux.moo.Unlock()
	if meth != nil {
		return meth.Call(s, args, depth)
	}
	// No matches on methods so call no-applicable-method (g, args...).
	nam := slip.MustFindFunc("no-applicable-method")

	return nam.Apply(s, append(slip.List{gf}, args...), depth)
}

// AddMethod adds a method to the Aux.
func (aux *Aux) AddMethod(key string, method *slip.Method) {
	aux.moo.Lock()
	aux.methods[key] = method
	if 0 < len(aux.cache) {
		aux.cache = map[string]*slip.Method{}
	}
	aux.updateDefaultCaller()
	aux.moo.Unlock()
}

func (aux *Aux) updateDefaultCaller() {
	aux.defaultCaller = nil
	if len(aux.methods) == 1 {
		for k, m := range aux.methods {
			if len(m.Combinations) == 1 && aux.defaultKey == k {
				c := m.Combinations[0]
				if c.Primary != nil && c.Before == nil && c.After == nil && c.Wrap == nil {
					aux.defaultCaller = c.Primary
				}
			}
		}
	}
}

// LoadForm returns a list that can be evaluated to define a generic and all
// specialized methods for the generic.
func (aux *Aux) LoadForm() slip.Object {
	ll := make(slip.List, len(aux.docs.Args))
	for i, da := range aux.docs.Args {
		ll[i] = slip.Symbol(da.Name)
	}
	gdef := slip.List{
		slip.Symbol("defgeneric"),
		slip.Symbol(aux.docs.Name),
		ll,
	}
	if 0 < len(aux.docs.Text) {
		gdef = append(gdef, slip.List{slip.Symbol(":documentation"), slip.String(aux.docs.Text)})
	}
	keys := make([]string, 0, len(aux.methods))
	for k := range aux.methods {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, k := range keys {
		method := aux.methods[k]
		sll := make(slip.List, len(method.Doc.Args))
		for i, da := range method.Doc.Args {
			if i < aux.reqCnt {
				sll[i] = slip.List{slip.Symbol(da.Name), slip.Symbol(da.Type)}
			} else {
				if da.Name[0] == '&' || da.Default == nil {
					sll[i] = slip.Symbol(da.Name)
				} else {
					sll[i] = slip.List{slip.Symbol(da.Name), da.Default}
				}
			}
		}
		var doc slip.Object
		if 0 < len(aux.docs.Text) {
			doc = slip.String(aux.docs.Text)
		}
		if 0 < len(method.Doc.Text) {
			doc = slip.String(method.Doc.Text)
		}
		if 0 < len(method.Combinations) {
			if lam, ok := method.Combinations[0].Primary.(*slip.Lambda); ok {
				mdef := slip.List{slip.Symbol(":method"), sll}
				if doc != nil {
					mdef = append(mdef, doc)
				}
				mdef = append(mdef, lam.Forms...)
				gdef = append(gdef, mdef)
			}
			if lam, ok := method.Combinations[0].Before.(*slip.Lambda); ok {
				mdef := slip.List{slip.Symbol(":method"), slip.Symbol(":before"), sll}
				if doc != nil {
					mdef = append(mdef, doc)
				}
				mdef = append(mdef, lam.Forms...)
				gdef = append(gdef, mdef)
			}
			if lam, ok := method.Combinations[0].After.(*slip.Lambda); ok {
				mdef := slip.List{slip.Symbol(":method"), slip.Symbol(":after"), sll}
				if doc != nil {
					mdef = append(mdef, doc)
				}
				mdef = append(mdef, lam.Forms...)
				gdef = append(gdef, mdef)
			}
			if lam, ok := method.Combinations[0].Wrap.(*slip.Lambda); ok {
				mdef := slip.List{slip.Symbol(":method"), slip.Symbol(":around"), sll}
				if doc != nil {
					mdef = append(mdef, doc)
				}
				mdef = append(mdef, lam.Forms...)
				gdef = append(gdef, mdef)
			}
		}
	}
	return gdef
}

func (aux *Aux) buildCacheMeth(args slip.List) *slip.Method {
	var meth slip.Method
	key := make([]string, aux.reqCnt)
	aux.collectMethods(&meth, key, 0, args)
	if len(meth.Combinations) == 0 {
		return nil
	}
	meth.Name = aux.docs.Name
	meth.Doc = aux.docs

	return &meth
}

func (aux *Aux) collectMethods(meth *slip.Method, key []string, ki int, args slip.List) {
	var hier []slip.Symbol
	if args[ki] == nil {
		hier = []slip.Symbol{slip.TrueSymbol}
	} else {
		hier = args[ki].Hierarchy()
	}
	if len(key) == ki+1 { // last one
		for _, h := range hier {
			key[ki] = string(h)
			if m, has := aux.methods[strings.Join(key, "|")]; has {
				meth.Combinations = append(meth.Combinations, m.Combinations...) // should only be one
			}
		}
	} else {
		k2 := ki + 1
		for _, h := range hier {
			key[ki] = string(h)
			aux.collectMethods(meth, key, k2, args)
		}
	}
}

type methComp struct {
	primary *slip.Method
	around  []*slip.Method
	before  []*slip.Method
	after   []*slip.Method
}

func (aux *Aux) compMethList(args slip.List) slip.List {
	var mc methComp
	key := make([]string, aux.reqCnt)

	aux.compMeths(&mc, key, 0, args)

	methods := make(slip.List, 0, len(mc.around)+len(mc.before)+len(mc.after)+1)
	for _, m := range mc.around {
		methods = append(methods, m)
	}
	for _, m := range mc.before {
		methods = append(methods, m)
	}
	if mc.primary != nil {
		methods = append(methods, mc.primary)
	}
	for i := len(mc.after) - 1; 0 <= i; i-- {
		methods = append(methods, mc.after[i])
	}
	return methods
}

func (aux *Aux) compMeths(mc *methComp, key []string, ki int, args slip.List) {
	var hier []slip.Symbol
	if args[ki] == nil {
		hier = []slip.Symbol{slip.TrueSymbol}
	} else {
		hier = args[ki].Hierarchy()
	}
	if len(key) == ki+1 { // last one
		for _, h := range hier {
			key[ki] = string(h)
			if m, has := aux.methods[strings.Join(key, "|")]; has {
				c := m.Combinations[0] // should only be one
				if mc.primary == nil && c.Primary != nil {
					mc.primary = &slip.Method{
						Name:         m.Name,
						Combinations: []*slip.Combination{{Primary: c.Primary}},
						Doc:          m.Doc,
					}
				}
				if c.Before != nil {
					mc.before = append(mc.before,
						&slip.Method{
							Name:         m.Name,
							Combinations: []*slip.Combination{{Before: c.Before}},
							Doc:          m.Doc,
						})
				}
				if c.After != nil {
					mc.after = append(mc.after,
						&slip.Method{
							Name:         m.Name,
							Combinations: []*slip.Combination{{After: c.After}},
							Doc:          m.Doc,
						})
				}
				if c.Wrap != nil {
					mc.around = append(mc.around,
						&slip.Method{
							Name:         m.Name,
							Combinations: []*slip.Combination{{Wrap: c.Wrap}},
							Doc:          m.Doc,
						})
				}
			}
		}
	} else {
		k2 := ki + 1
		for _, h := range hier {
			key[ki] = string(h)
			aux.compMeths(mc, key, k2, args)
		}
	}
}

// args should be only specializer arguments.
func buildSpecKey(args slip.List) string {
	var b []byte
	for i, a := range args {
		if 0 < i {
			b = append(b, '|')
		}
		if a == nil {
			b = append(b, 't')
		} else {
			b = append(b, a.Hierarchy()[0]...)
		}
	}
	return string(b)
}
