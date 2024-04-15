// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

type reader interface {
	initialize()
	stop()
	reset()
	setDepth(d int)
	read() []byte
	addToHistory()
	afterEval()
}
