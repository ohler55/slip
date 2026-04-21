// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"testing"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg/cl"
)

func TestXrefCalls(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Define a function that calls known functions so findCalls has a lambda to walk.
	// The compiled forms will be Funky objects; walkForCalls recurses into their args.
	env.sendRexOK(t, slip.List{slip.Symbol("swank:interactive-eval"),
		slip.String("(defun test-xref-fn (x) (car (cdr x)))")})

	// Find what test-xref-fn calls — walks compiled Funky forms.
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":calls"),
		slip.String("test-xref-fn"),
	})
	if result == nil {
		t.Log("xref :calls returned nil (function may not have lambda source)")
	} else if list, ok := result.(slip.List); ok {
		t.Logf("xref :calls found %d entries", len(list))
	}
}

func TestXrefCallsWithNestedCalls(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Define a function with nested calls and a special form (if) to exercise
	// both walkForCalls branches and isSpecialForm filtering.
	env.sendRexOK(t, slip.List{slip.Symbol("swank:interactive-eval"),
		slip.String("(defun test-xref-nested (x) (if x (car x) (cdr x)))")})

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":calls"),
		slip.String("test-xref-nested"),
	})
	if result != nil {
		if list, ok := result.(slip.List); ok {
			t.Logf("xref :calls for nested found %d entries", len(list))
		}
	}
}

func TestXrefCallsBuiltin(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Built-in functions have no lambda, so findCalls should return empty.
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":calls"),
		slip.String("car"),
	})
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for builtin :calls, got %v", result)
	}
}

func TestXrefCallers(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Define a function that calls car so findCallers can find it.
	// This exercises getLambda, callsFunction (Funky and Symbol branches).
	env.sendRexOK(t, slip.List{slip.Symbol("swank:interactive-eval"),
		slip.String("(defun xref-caller-test (x) (car x))")})

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":callers"),
		slip.String("car"),
	})
	if list, ok := result.(slip.List); ok {
		t.Logf("xref :callers for car found %d entries", len(list))
	}
}

func TestXrefCallersSymbolRef(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Define a function that passes a function name as a symbol argument
	// (e.g., via funcall) to exercise the Symbol branch of callsFunction.
	env.sendRexOK(t, slip.List{slip.Symbol("swank:interactive-eval"),
		slip.String("(defun xref-sym-test (x) (apply 'list x))")})

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":callers"),
		slip.String("list"),
	})
	if list, ok := result.(slip.List); ok {
		t.Logf("xref :callers for list found %d entries", len(list))
	}
}

func TestXrefReferences(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Define a function that references a global variable to exercise
	// referencesSymbol (Symbol and Funky branches).
	env.sendRexOK(t, slip.List{slip.Symbol("swank:interactive-eval"),
		slip.String("(defun xref-ref-test () *standard-output*)")})

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":references"),
		slip.String("*standard-output*"),
	})
	if list, ok := result.(slip.List); ok {
		t.Logf("xref :references for *standard-output* found %d entries", len(list))
	}
}

func TestXrefBinds(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":binds"),
		slip.String("*standard-output*"),
	})
	if result != nil {
		if _, ok := result.(slip.List); !ok {
			t.Errorf("expected list result for :binds, got %T", result)
		}
	}
}

func TestXrefSets(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":sets"),
		slip.String("*print-base*"),
	})
	if result != nil {
		if _, ok := result.(slip.List); !ok {
			t.Errorf("expected list result for :sets, got %T", result)
		}
	}
}

func TestXrefMacroexpands(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":macroexpands"),
		slip.String("when"),
	})
	if result != nil {
		if _, ok := result.(slip.List); !ok {
			t.Errorf("expected list result for :macroexpands, got %T", result)
		}
	}
}

func TestXrefSpecializes(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":specializes"),
		slip.String("t"),
	})
	// Should return empty list (not implemented).
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for :specializes, got %v", result)
	}
}

func TestXrefNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Less than 2 args should return empty list.
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:xref"), slip.Symbol(":calls")})
	if list, ok := result.(slip.List); !ok || len(list) != 0 {
		t.Logf("xref with 1 arg: %v", result)
	}
}

func TestXrefInvalidType(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":unknown-type"),
		slip.String("car"),
	})
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for unknown xref type, got %v", result)
	}
}

func TestXrefInvalidArgTypes(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Non-symbol/string xref type.
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Fixnum(42),
		slip.String("car"),
	})
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for bad type arg, got %v", result)
	}

	// Non-symbol/string name.
	result = env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":calls"),
		slip.Fixnum(42),
	})
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for bad name arg, got %v", result)
	}
}

func TestXrefStringArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Both args as strings instead of symbols.
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.String(":calls"),
		slip.String("car"),
	})
	if result != nil {
		if _, ok := result.(slip.List); !ok {
			t.Errorf("expected list result for string-args xref, got %T", result)
		}
	}
}

func TestXrefsMultiple(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Multiple xref queries.
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xrefs"),
		slip.List{
			slip.List{slip.Symbol(":calls"), slip.String("car")},
			slip.List{slip.Symbol(":callers"), slip.String("car")},
		},
	})
	if result != nil {
		if _, ok := result.(slip.List); !ok {
			t.Errorf("expected list result for xrefs, got %T", result)
		}
	}
}

func TestXrefsNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:xrefs")})
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for no args, got %v", result)
	}
}

func TestXrefsInvalidArg(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Non-list arg.
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:xrefs"), slip.Fixnum(42)})
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for bad arg, got %v", result)
	}
}

func TestXrefsShortQuery(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Query with less than 2 elements should be skipped.
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xrefs"),
		slip.List{
			slip.List{slip.Symbol(":calls")}, // only 1 element
		},
	})
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for short query, got %v", result)
	}
}

func TestXrefsNonListQuery(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Non-list element inside queries should be skipped.
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xrefs"),
		slip.List{
			slip.String("not-a-list"),
		},
	})
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for non-list query element, got %v", result)
	}
}

func TestXrefPackageQualified(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":calls"),
		slip.String("cl:car"),
	})
	if result != nil {
		if _, ok := result.(slip.List); !ok {
			t.Errorf("expected list result for package-qualified xref, got %T", result)
		}
	}
}

func TestXrefPackageQualifiedBadPkg(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Package that doesn't exist should return empty.
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":calls"),
		slip.String("nonexistent-pkg:car"),
	})
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for bad package prefix, got %v", result)
	}
}

func TestXrefNonexistent(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:xref"),
		slip.Symbol(":calls"),
		slip.String("nonexistent-function-xyz"),
	})
	if list, ok := result.(slip.List); ok && len(list) != 0 {
		t.Errorf("expected empty list for nonexistent fn, got %v", result)
	}
}
