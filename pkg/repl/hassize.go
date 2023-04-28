// Copyright (c) 2023, Peter Ohler, All rights reserved.

package repl

type hasSize interface {
	getSize() (w, h int)
}
