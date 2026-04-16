// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg/cl" // load CL functions
)

// getSymbolFlags is private and its package-prefix branches
// (name ending with ":") are unreachable via fuzzy-completions,
// since findCompletions never emits bare "pkg:" entries. These
// tests exercise the branches directly.

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

// formatFuncDescription's Kind switch has MacroSymbol and
// GenericFunctionSymbol arms that weren't exercised by the
// integration tests. Build a bare FuncInfo and call directly.

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

// getObjectTitle's nil guard.
func TestGetObjectTitleNil(t *testing.T) {
	if s := getObjectTitle(nil); s != "nil" {
		t.Errorf("expected 'nil', got %q", s)
	}
}

// getObjectType has three paths: nil -> "null", non-empty hierarchy,
// and the fall-through "object" default. Cover nil and the empty-
// hierarchy fallback (via a type that returns []Symbol{}).
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

// referencesSymbol's recursion branches: match inside a nested list,
// and traversal of Funky args.

func TestReferencesSymbolInNestedList(t *testing.T) {
	// The symbol is two levels deep; the outer list recursion must
	// dive in and the match must return true.
	form := slip.List{slip.Symbol("foo"), slip.List{slip.Symbol("bar"), slip.Symbol("target")}}
	if !referencesSymbol(form, "target") {
		t.Error("expected to find target in nested list")
	}
	if referencesSymbol(form, "nope") {
		t.Error("did not expect to find missing symbol")
	}
}

// callsFunction's recursive match branch is entered when a call lives
// inside a subform (not at the head of the top-level list).

func TestCallsFunctionInSubform(t *testing.T) {
	form := slip.List{slip.Symbol("if"), slip.Symbol("x"),
		slip.List{slip.Symbol("car"), slip.Symbol("x")}, nil}
	if !callsFunction(form, "car") {
		t.Error("expected to find car in nested subform")
	}
}

// walkForCalls should early-return on an empty list without recursing
// or appending anything.

func TestWalkForCallsEmptyList(t *testing.T) {
	seen := map[string]bool{}
	var results slip.List
	walkForCalls(slip.List{}, seen, &results)
	if len(results) != 0 || len(seen) != 0 {
		t.Errorf("expected no-op on empty list, got results=%v seen=%v", results, seen)
	}
}

// Same early-return guard in callsFunction.

func TestCallsFunctionEmptyList(t *testing.T) {
	if callsFunction(slip.List{}, "anything") {
		t.Error("empty list should not call anything")
	}
}

// getLambda must handle a FuncInfo whose Create is nil without
// panicking.

func TestGetLambdaNilCreate(t *testing.T) {
	fi := &slip.FuncInfo{Name: "no-create", Create: nil}
	if lam := getLambda(fi); lam != nil {
		t.Errorf("expected nil lambda for nil Create, got %v", lam)
	}
}

// findFunction returns the match from a used package when the name
// is not in the current package. cl-user uses cl, so "mapcar" (in cl)
// reached from cl-user exercises the used-packages loop body.

func TestFindFunctionFromUsedPackage(t *testing.T) {
	c := &Connection{currentPkg: slip.FindPackage("cl-user")}
	fi := findFunction(c, "mapcar")
	if fi == nil {
		t.Fatal("expected to find mapcar via cl-user.Uses")
	}
	if fi.Name != "mapcar" {
		t.Errorf("expected mapcar, got %q", fi.Name)
	}
}
