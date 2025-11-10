// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build !aix && !darwin && !dragonfly && !freebsd && !linux && !netbsd && !openbsd && !zos && !windows && !solaris

package term

import (
	"fmt"
	"runtime"
)

type state struct{}

func isTerminal(fd int) bool {
	return false
}

func makeRaw(fd int) *State {
	panic(fmt.Errorf("terminal: MakeRaw not implemented on %s/%s", runtime.GOOS, runtime.GOARCH))
}

func getState(fd int) *State {
	panic(fmt.Errorf("terminal: GetState not implemented on %s/%s", runtime.GOOS, runtime.GOARCH))
}

func restore(fd int, state *State) {
	panic(fmt.Errorf("terminal: Restore not implemented on %s/%s", runtime.GOOS, runtime.GOARCH))
}

func getSize(fd int) (width, height int) {
	panic(fmt.Errorf("terminal: GetSize not implemented on %s/%s", runtime.GOOS, runtime.GOARCH))
}
