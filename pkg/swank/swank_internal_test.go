// Copyright (c) 2025, Peter Ohler, All rights reserved.

// Internal tests for swank helpers whose branches are not reachable
// through the public RPC dispatch. Exporting these helpers would bloat
// the package API for no external benefit, so the tests live inside
// package swank and exercise each branch directly.
//
// Each test pairs a helper with the specific branch it covers:
//
//   getSymbolFlags         — package-prefix ("pkg:") branches.
//                            findCompletions never emits bare "pkg:"
//                            entries, so external completion tests can
//                            never reach them. Also the used-packages
//                            traversal for a symbol inherited from a
//                            :use'd package.
//
//   formatFuncDescription  — the MacroSymbol / GenericFunctionSymbol /
//                            MethodSymbol arms. The describe RPC only
//                            surfaces regular functions in the current
//                            fixture set, so these arms are untouched
//                            by external tests.
//
//   getObjectTitle /       — nil guards and the empty-Hierarchy()
//   getObjectType            fall-through. The inspector RPC never
//                            presents a nil target or an object with
//                            zero hierarchy, so these defensive paths
//                            are internal-only.
//
//   referencesSymbol /     — empty-list early returns and nested-list
//   callsFunction /          recursion. xref fixtures don't naturally
//   walkForCalls             produce these shapes.
//
//   getLambda              — nil-Create guard for FuncInfo entries
//                            that lack a Create factory. Built-ins
//                            always set one, so no public call reaches
//                            this branch.

package swank

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg/cl" // load CL functions
)

func TestGetSymbolFlagsPackageKnown(t *testing.T) {
	c := &Connection{currentPkg: slip.FindPackage("cl-user")}
	flags := getSymbolFlags(c, "cl:")
	// Position 7 (p) should be set for a valid package.
	if len(flags) != 8 || flags[7] != 'p' {
		t.Errorf("expected p flag at pos 7 for cl:, got %q", flags)
	}
	// Other positions should remain '-'.
	for i := 0; i < 7; i++ {
		if flags[i] != '-' {
			t.Errorf("unexpected flag at pos %d: %q", i, flags)
		}
	}
}

func TestGetSymbolFlagsPackageUnknown(t *testing.T) {
	c := &Connection{currentPkg: slip.FindPackage("cl-user")}
	flags := getSymbolFlags(c, "no-such-pkg:")
	// Unknown package should leave all flags unset.
	if flags != "--------" {
		t.Errorf("expected all-dashes for unknown package, got %q", flags)
	}
}

func TestGetSymbolFlagsUsedPackageFunction(t *testing.T) {
	// cl-user uses cl, so looking up a cl function like "car" from
	// cl-user must traverse the used-packages loop and set the 'f' flag.
	c := &Connection{currentPkg: slip.FindPackage("cl-user")}
	flags := getSymbolFlags(c, "car")
	if len(flags) != 8 || flags[1] != 'f' {
		t.Errorf("expected f flag at pos 1 for car, got %q", flags)
	}
}

func TestFormatFuncDescriptionMacro(t *testing.T) {
	fi := &slip.FuncInfo{Name: "my-macro", Kind: slip.MacroSymbol}
	out := formatFuncDescription(fi)
	if !strings.Contains(out, "names a macro") {
		t.Errorf("expected 'names a macro', got %q", out)
	}
}

func TestFormatFuncDescriptionGeneric(t *testing.T) {
	fi := &slip.FuncInfo{Name: "my-gf", Kind: slip.GenericFunctionSymbol}
	out := formatFuncDescription(fi)
	if !strings.Contains(out, "names a generic function") {
		t.Errorf("expected 'names a generic function', got %q", out)
	}
}

func TestFormatFuncDescriptionMethod(t *testing.T) {
	fi := &slip.FuncInfo{Name: "my-method", Kind: slip.MethodSymbol}
	out := formatFuncDescription(fi)
	if !strings.Contains(out, "names a method") {
		t.Errorf("expected 'names a method', got %q", out)
	}
}

func TestGetObjectTitleNil(t *testing.T) {
	if s := getObjectTitle(nil); s != "nil" {
		t.Errorf("expected 'nil', got %q", s)
	}
}

func TestGetObjectTypeNil(t *testing.T) {
	if s := getObjectType(nil); s != "null" {
		t.Errorf("expected 'null', got %q", s)
	}
}

// emptyHierObj implements slip.Object with an empty Hierarchy() slice
// so we can reach the fall-through in getObjectType. All other methods
// are stubs; only Hierarchy and the interface tag matter for the call.
type emptyHierObj struct{}

func (emptyHierObj) String() string                    { return "" }
func (emptyHierObj) Append(b []byte) []byte            { return b }
func (emptyHierObj) Simplify() any                     { return nil }
func (emptyHierObj) Equal(slip.Object) bool            { return false }
func (emptyHierObj) Hierarchy() []slip.Symbol          { return nil }
func (emptyHierObj) Eval(*slip.Scope, int) slip.Object { return nil }

func TestGetObjectTypeFallthrough(t *testing.T) {
	if s := getObjectType(emptyHierObj{}); s != "object" {
		t.Errorf("expected 'object' fallthrough, got %q", s)
	}
}

func TestReferencesSymbolInNestedList(t *testing.T) {
	form := slip.List{slip.Symbol("foo"), slip.List{slip.Symbol("bar"), slip.Symbol("target")}}
	if !referencesSymbol(form, "target") {
		t.Error("expected to find target in nested list")
	}
	if referencesSymbol(form, "nope") {
		t.Error("did not expect to find missing symbol")
	}
}

func TestCallsFunctionInSubform(t *testing.T) {
	form := slip.List{slip.Symbol("if"), slip.Symbol("x"),
		slip.List{slip.Symbol("car"), slip.Symbol("x")}, nil}
	if !callsFunction(form, "car") {
		t.Error("expected to find car in nested subform")
	}
}

func TestWalkForCallsEmptyList(t *testing.T) {
	seen := map[string]bool{}
	var results slip.List
	walkForCalls(slip.List{}, seen, &results)
	if len(results) != 0 || len(seen) != 0 {
		t.Errorf("expected no-op on empty list, got results=%v seen=%v", results, seen)
	}
}

func TestCallsFunctionEmptyList(t *testing.T) {
	if callsFunction(slip.List{}, "anything") {
		t.Error("empty list should not call anything")
	}
}

func TestGetLambdaNilCreate(t *testing.T) {
	fi := &slip.FuncInfo{Name: "no-create", Create: nil}
	if lam := getLambda(fi); lam != nil {
		t.Errorf("expected nil lambda for nil Create, got %v", lam)
	}
}
