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
	cache   map[string]*slip.Method
	methods map[string]*slip.Method
	moo     sync.Mutex
}

// NewAux creates a new generic aux.
func NewAux(fd *slip.FuncDoc) *Aux {
	var cnt int
	for _, da := range fd.Args {
		if da.Name[0] == '&' {
			break
		}
		cnt++
	}
	return &Aux{
		cache:   map[string]*slip.Method{},
		methods: map[string]*slip.Method{},
		docs:    fd,
		reqCnt:  cnt,
	}
}

// Call the the function with the arguments provided.
func (g *Aux) Call(gf slip.Object, s *slip.Scope, args slip.List, depth int) slip.Object {
	// All calls must match the gf-lambda-list so check here as much as
	// reasonable.
	if len(args) < g.reqCnt {
		slip.NewPanic("generic-function %s requires at least %d arguments. Received %d.",
			g.docs.Name, g.reqCnt, len(args))
	}
	// Any further argument checking gets tricky as optinal could be keywords
	// depending on then method's forms.
	key := buildSpecKey(args[:g.reqCnt])
	g.moo.Lock()
	meth := g.cache[key]
	if meth == nil {
		if meth = g.buildCacheMeth(args); meth != nil {
			g.cache[key] = meth
		}
	}
	g.moo.Unlock()
	if meth != nil {
		return meth.Call(s, args, depth)
	}
	// No matches on methods so call no-applicable-method (g, args...).
	nam := slip.MustFindFunc("no-applicable-method")

	return nam.Apply(s, append(slip.List{gf}, args...), depth)
}

// DefList returns a list that can be evaluated to define a generic and all
// specialized methods for the generic.
func (g *Aux) DefList() slip.List {
	ll := make(slip.List, len(g.docs.Args))
	for i, da := range g.docs.Args {
		ll[i] = slip.Symbol(da.Name)
	}
	gdef := slip.List{
		slip.Symbol("defgeneric"),
		slip.Symbol(g.docs.Name),
		ll,
	}
	if 0 < len(g.docs.Text) {
		gdef = append(gdef, slip.List{slip.Symbol(":documentation"), slip.String(g.docs.Text)})
	}
	keys := make([]string, 0, len(g.methods))
	for k := range g.methods {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, k := range keys {
		method := g.methods[k]
		sll := make(slip.List, len(method.Doc.Args))
		for i, da := range method.Doc.Args {
			if i < g.reqCnt {
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
		if 0 < len(g.docs.Text) {
			doc = slip.String(g.docs.Text)
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

func (g *Aux) buildCacheMeth(args slip.List) *slip.Method {
	var meth slip.Method
	key := make([]string, g.reqCnt)
	g.collectMethods(&meth, key, 0, args)
	if len(meth.Combinations) == 0 {
		return nil
	}
	meth.Name = g.docs.Name
	meth.Doc = g.docs

	return &meth
}

func (g *Aux) collectMethods(meth *slip.Method, key []string, ki int, args slip.List) {
	var hier []slip.Symbol
	if args[ki] == nil {
		hier = []slip.Symbol{slip.TrueSymbol}
	} else {
		hier = args[ki].Hierarchy()
	}
	if len(key) == ki+1 { // last one
		for _, h := range hier {
			key[ki] = string(h)
			if m, has := g.methods[strings.Join(key, "|")]; has {
				meth.Combinations = append(meth.Combinations, m.Combinations...) // should only be one
			}
		}
	} else {
		k2 := ki + 1
		for _, h := range hier {
			key[ki] = string(h)
			g.collectMethods(meth, key, k2, args)
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
