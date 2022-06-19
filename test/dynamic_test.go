// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDynamicLambda(t *testing.T) {
	key := slip.Symbol("*print-lambda*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.True)

	lambda := &slip.Dynamic{
		Function: slip.Function{
			Args: slip.List{slip.Fixnum(2), slip.NewFunc("quote", slip.List{slip.List{nil, slip.Fixnum(1)}})},
		},
	}
	lambda.Self = &slip.Lambda{
		Doc: &slip.FuncDoc{
			Args: []*slip.DocArg{{Name: "x"}, {Name: "y"}},
		},
		Forms: slip.List{slip.NewFunc("car", slip.List{slip.Symbol("x")}), nil},
	}
	(&sliptest.Object{
		Target: lambda,
		String: "((lambda (x y) nil (car x)) '(1 nil) 2)",
		Simple: sen.MustParse([]byte(`[
                                       [lambda [x y] null [car x]]
                                       [quote [1 null]]
                                       2]`)),
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: lambda, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.Fixnum(1),
	}).Test(t)
}

func TestDynamicDefun(t *testing.T) {
	key := slip.Symbol("*print-lambda*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.True)
	dummy := &slip.Dynamic{
		Function: slip.Function{
			Name: "dummy",
			Args: slip.List{slip.Fixnum(2), slip.NewFunc("quote", slip.List{slip.List{nil, slip.Fixnum(1)}})},
		},
	}
	dummy.Self = &slip.Lambda{
		Doc: &slip.FuncDoc{
			Args: []*slip.DocArg{{Name: "x"}, {Name: "y"}},
		},
		Forms: slip.List{slip.NewFunc("car", slip.List{slip.Symbol("x")}), nil},
	}
	(&sliptest.Object{
		Target:    dummy,
		String:    "(dummy '(1 nil) 2)",
		Simple:    sen.MustParse([]byte(`[dummy [quote [1 null]] 2]`)),
		Hierarchy: "function.t",
		Eval:      slip.Fixnum(1),
	}).Test(t)
}

func TestDynamicAmps(t *testing.T) {
	key := slip.Symbol("*print-lambda*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.True)

	lambda := &slip.Dynamic{
		Function: slip.Function{
			Args: slip.List{slip.Fixnum(5), slip.Symbol(":k1"), slip.Fixnum(4), slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)},
		},
	}
	lambda.Self = &slip.Lambda{
		Doc: &slip.FuncDoc{
			Args: []*slip.DocArg{
				{Name: "x"},
				{Name: "&optional"}, {Name: "y"},
				{Name: "&rest"}, {Name: "z"},
				{Name: "&key"}, {Name: "k1"}, {Name: "k2"},
			},
		},
		Forms: slip.List{slip.NewFunc("list",
			slip.List{slip.Symbol("k2"), slip.Symbol("k1"), slip.Symbol("z"), slip.Symbol("y"), slip.Symbol("x")})},
	}
	(&sliptest.Object{
		Target: lambda,
		String: "((lambda (x &optional y &rest z &key k1 k2) (list x y z k1 k2)) 1 2 3 4 :k1 5)",
		Simple: sen.MustParse(
			[]byte(`[[lambda [x "&optional" y "&rest" z "&key" k1 k2] [list x y z k1 k2]] 1 2 3 4 ":k1" 5]`)),
		Hierarchy: "function.t",
		Eval: slip.List{
			nil, slip.Fixnum(5), slip.List{slip.Fixnum(4), slip.Fixnum(3)}, slip.Fixnum(2), slip.Fixnum(1)},
	}).Test(t)
}

func TestDynamicAmpRest(t *testing.T) {
	key := slip.Symbol("*print-lambda*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.True)

	lambda := &slip.Dynamic{
		Function: slip.Function{
			Args: slip.List{slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)},
		},
	}
	lambda.Self = &slip.Lambda{
		Doc: &slip.FuncDoc{
			Args: []*slip.DocArg{
				{Name: "x"},
				{Name: "&rest"}, {Name: "z"},
			},
		},
		Forms: slip.List{slip.NewFunc("list", slip.List{slip.Symbol("z"), slip.Symbol("x")})},
	}
	(&sliptest.Object{
		Target:    lambda,
		String:    "((lambda (x &rest z) (list x z)) 1 2 3)",
		Simple:    sen.MustParse([]byte(`[[lambda [x "&rest" z] [list x z]] 1 2 3]`)),
		Hierarchy: "function.t",
		Eval:      slip.List{slip.List{slip.Fixnum(3), slip.Fixnum(2)}, slip.Fixnum(1)},
	}).Test(t)
}

func TestDynamicAmpKey(t *testing.T) {
	key := slip.Symbol("*print-lambda*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.True)

	lambda := &slip.Dynamic{
		Function: slip.Function{
			Args: slip.List{slip.Fixnum(2), slip.Symbol(":k1"), slip.Fixnum(1)},
		},
	}
	lambda.Self = &slip.Lambda{
		Doc: &slip.FuncDoc{
			Args: []*slip.DocArg{
				{Name: "x"},
				{Name: "&key"}, {Name: "k1"},
			},
		},
		Forms: slip.List{slip.NewFunc("list", slip.List{slip.Symbol("k1"), slip.Symbol("x")})},
	}
	(&sliptest.Object{
		Target:    lambda,
		String:    "((lambda (x &key k1) (list x k1)) 1 :k1 2)",
		Simple:    sen.MustParse([]byte(`[[lambda [x "&key" k1] [list x k1]] 1 ":k1" 2]`)),
		Hierarchy: "function.t",
		Eval:      slip.List{slip.Fixnum(2), slip.Fixnum(1)},
	}).Test(t)
}

func TestDynamicAmpOptKey(t *testing.T) {
	key := slip.Symbol("*print-lambda*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.True)

	lambda := &slip.Dynamic{
		Function: slip.Function{
			Args: slip.List{slip.Fixnum(2), slip.Symbol(":k1"), slip.Fixnum(1)},
		},
	}
	lambda.Self = &slip.Lambda{
		Doc: &slip.FuncDoc{
			Args: []*slip.DocArg{
				{Name: "&optional"}, {Name: "x"},
				{Name: "&key"}, {Name: "k1"},
			},
		},
		Forms: slip.List{slip.NewFunc("list", slip.List{slip.Symbol("k1"), slip.Symbol("x")})},
	}
	(&sliptest.Object{
		Target:    lambda,
		String:    "((lambda (&optional x &key k1) (list x k1)) 1 :k1 2)",
		Simple:    sen.MustParse([]byte(`[[lambda ["&optional" x "&key" k1] [list x k1]] 1 ":k1" 2]`)),
		Hierarchy: "function.t",
		Eval:      slip.List{slip.Fixnum(2), slip.Fixnum(1)},
	}).Test(t)
}

func TestDynamicAmpDefaults(t *testing.T) {
	key := slip.Symbol("*print-lambda*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.True)

	lambda := &slip.Dynamic{
		Function: slip.Function{
			Args: slip.List{},
		},
	}
	lambda.Self = &slip.Lambda{
		Doc: &slip.FuncDoc{
			Args: []*slip.DocArg{
				{Name: "&optional"}, {Name: "x"},
				{Name: "&rest"}, {Name: "z"},
			},
		},
		Forms: slip.List{slip.NewFunc("list", slip.List{slip.Symbol("z"), slip.Symbol("x")})},
	}
	(&sliptest.Object{
		Target:    lambda,
		String:    "((lambda (&optional x &rest z) (list x z)))",
		Simple:    sen.MustParse([]byte(`[[lambda ["&optional" x "&rest" z] [list x z]]]`)),
		Hierarchy: "function.t",
		Eval:      slip.List{nil, nil},
	}).Test(t)
}

func TestDynamicAmpNoKeyValue(t *testing.T) {
	key := slip.Symbol("*print-lambda*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.True)

	lambda := &slip.Dynamic{
		Function: slip.Function{
			Args: slip.List{slip.Symbol(":k1"), slip.Fixnum(1)},
		},
	}
	lambda.Self = &slip.Lambda{
		Doc: &slip.FuncDoc{
			Args: []*slip.DocArg{
				{Name: "&optional"}, {Name: "x"},
				{Name: "&key"}, {Name: "k1"},
			},
		},
		Forms: slip.List{slip.NewFunc("list", slip.List{slip.Symbol("k1"), slip.Symbol("x")})},
	}
	(&sliptest.Object{
		Target:    lambda,
		String:    "((lambda (&optional x &key k1) (list x k1)) 1 :k1)",
		Simple:    sen.MustParse([]byte(`[[lambda ["&optional" x "&key" k1] [list x k1]] 1 ":k1"]`)),
		Hierarchy: "function.t",
		Panics:    true,
	}).Test(t)
}

func TestDynamicAmpKeyExtraSymbol(t *testing.T) {
	key := slip.Symbol("*print-lambda*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.True)

	lambda := &slip.Dynamic{
		Function: slip.Function{
			Args: slip.List{slip.Fixnum(3), slip.Fixnum(2), slip.Symbol(":k1"), slip.Fixnum(1)},
		},
	}
	lambda.Self = &slip.Lambda{
		Doc: &slip.FuncDoc{
			Args: []*slip.DocArg{
				{Name: "&optional"}, {Name: "x"},
				{Name: "&key"}, {Name: "k1"},
			},
		},
		Forms: slip.List{slip.NewFunc("list", slip.List{slip.Symbol("k1"), slip.Symbol("x")})},
	}
	(&sliptest.Object{
		Target:    lambda,
		String:    "((lambda (&optional x &key k1) (list x k1)) 1 :k1 2 3)",
		Simple:    sen.MustParse([]byte(`[[lambda ["&optional" x "&key" k1] [list x k1]] 1 ":k1" 2 3]`)),
		Hierarchy: "function.t",
		Panics:    true,
	}).Test(t)
}
