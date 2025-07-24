// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestLockerTryLock(t *testing.T) {
	var locker slip.NoOpLocker

	tt.Equal(t, false, locker.TryLock())
}
