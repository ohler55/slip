// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"testing"

	"github.com/ohler55/slip/pkg/swank"
)

func TestHandlerListRegistration(t *testing.T) {
	// Test that ListHandlers works
	handlers := swank.ListHandlers()
	if len(handlers) == 0 {
		t.Error("expected registered handlers")
	}

	// Check some expected handlers exist
	expectedHandlers := []string{
		"swank:connection-info",
		"swank:create-repl",
		"swank:listener-eval",
	}

	for _, expected := range expectedHandlers {
		found := false
		for _, h := range handlers {
			if h == expected {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("expected handler %s to be registered", expected)
		}
	}
}
