// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package term

import (
	"golang.org/x/sys/windows"
)

type state struct {
	mode uint32
}

func isTerminal(fd int) bool {
	var st uint32
	err := windows.GetConsoleMode(windows.Handle(fd), &st)
	return err == nil
}

func makeRaw(fd int) *State {
	var st uint32
	if err := windows.GetConsoleMode(windows.Handle(fd), &st); err != nil {
		panic(err)
	}
	raw := st &^ (windows.ENABLE_ECHO_INPUT | windows.ENABLE_PROCESSED_INPUT | windows.ENABLE_LINE_INPUT | windows.ENABLE_PROCESSED_OUTPUT)
	if err := windows.SetConsoleMode(windows.Handle(fd), raw); err != nil {
		panic(err)
	}
	return &State{state{st}}
}

func getState(fd int) *State {
	var st uint32
	if err := windows.GetConsoleMode(windows.Handle(fd), &st); err != nil {
		panic(err)
	}
	return &State{state{st}}
}

func restore(fd int, state *State) {
	if err := windows.SetConsoleMode(windows.Handle(fd), state.mode); err != nil {
		panic(err)
	}
}

func getSize(fd int) (width, height int) {
	var info windows.ConsoleScreenBufferInfo
	if err := windows.GetConsoleScreenBufferInfo(windows.Handle(fd), &info); err != nil {
		panic(err)
	}
	return int(info.Window.Right - info.Window.Left + 1), int(info.Window.Bottom - info.Window.Top + 1)
}
