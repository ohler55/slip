// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
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
	var key []byte
	for i, a := range args {
		if g.reqCnt <= i {
			break
		}
		if 0 < i {
			key = append(key, '|')
		}
		if a == nil {
			key = append(key, 't')
		} else {
			key = append(key, a.Hierarchy()[0]...)
		}
	}
	g.moo.Lock()
	meth := g.cache[string(key)]
	if meth == nil {
		if meth = g.buildCacheMeth(args); meth != nil {
			g.cache[string(key)] = meth
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
