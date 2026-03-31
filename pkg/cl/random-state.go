// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"math/rand"
	"sync"
	"time"

	"github.com/ohler55/slip"
)

const (
	indicesCnt     = 30
	randomStateStr = "*random-state*"
)

// RandomStateSymbol is the symbol with a value of "random-state".
const RandomStateSymbol = slip.Symbol("random-state")

func init() {
	slip.CLPkg.Locked = false // a bit of a cheat
	slip.CLPkg.DefConst(string(RandomStateSymbol), RandomStateSymbol,
		`A _random-state_ stores the state for random number generation.`)
	slip.CLPkg.Set(randomStateStr, NewRandomState(nil))
	slip.CLPkg.Locked = true
	slip.CLPkg.Export(string(RandomStateSymbol))
}

// RandomState is rand.Source64.
type RandomState struct {
	src   rand.Source
	seed  int64
	count int64
	mu    sync.Mutex
}

// NewRandomState creates a new RandomState.
func NewRandomState(state *RandomState) *RandomState {
	rs := RandomState{
		seed: time.Now().UnixNano(),
	}
	if state != nil {
		// A bit of a cheat but it achieves the desired result for most cases.
		state.src.Seed(rs.seed)
		state.seed = rs.seed
		state.count = 0
	}
	rs.src = rand.NewSource(rs.seed)

	return &rs
}

// String representation of the Object.
func (obj *RandomState) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *RandomState) Append(b []byte) []byte {
	return fmt.Appendf(b, "#<random-state %d %d>", obj.seed, obj.count)
}

// Simplify the Object into an int64.
func (obj *RandomState) Simplify() interface{} {
	return string(obj.Append([]byte{}))
}

// Equal returns true if this Object and the other are equal in value.
func (obj *RandomState) Equal(other slip.Object) (eq bool) {
	if rs, ok := other.(*RandomState); ok && rs.seed == obj.seed && rs.count == obj.count {
		eq = true
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *RandomState) Hierarchy() []slip.Symbol {
	return []slip.Symbol{RandomStateSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj *RandomState) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Seed seeds the object by setting the indices of the state.
func (obj *RandomState) Seed(seed int64) {
	obj.mu.Lock()
	obj.seed = seed
	obj.count = 0
	obj.src.Seed(seed)
	obj.mu.Unlock()
}

// Int63 returns a random int64.
func (obj *RandomState) Int63() (r int64) {
	obj.mu.Lock()
	r = obj.src.Int63()
	obj.count++
	obj.mu.Unlock()
	return
}
