// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"testing"

	"github.com/ohler55/slip"
)

func TestSwankRequire(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:swank-require"),
		slip.String("swank-repl"),
	})
	// Should return empty list
	if list, ok := result.(slip.List); !ok || len(list) != 0 {
		t.Errorf("expected empty list, got %v", result)
	}
}

func TestSwankRequireMultiple(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:swank-require"),
		slip.List{slip.String("swank-repl"), slip.String("swank-presentations")},
	})
	if list, ok := result.(slip.List); !ok || len(list) != 0 {
		t.Errorf("expected empty list, got %v", result)
	}
}
