// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"sync"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WithMutexLock{
				Function: slip.Function{Name: "with-mutex-lock", Args: args, SkipEval: []bool{false, true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "with-mutex-lock",
			Args: []*slip.DocArg{
				{
					Name: "mutex",
					Type: "mutex",
					Text: `A mutex to synchronize processing with.`,
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__with-mutex-lock__ protects the _forms_ with a _mutex_ that is locked on entry and
unlocked when the forms complete or panic. In many cases use of a __channel__ and threads are a better
choice than using a mutex.`,
			Examples: []string{
				`(let ((m (make-mutex))`,
				`      (done (make-channel 1))`,
				`      lst)`,
				`  (run (with-mutex-lock m`,
				`         (setq lst (add lst 'other))`,
				`         (channel-push done t)`,
				`         (channel-close done)))`,
				`  (with-mutex-lock m`,
				`    (sleep 0.1) ;; enough for the run to get started most of the time`,
				`    (setq lst (add lst 'main)))`,
				`  (channel-pop done)`,
				`  lst) => (main other)`,
			},
		}, &Pkg)
}

// WithMutexLock represents the with-mutex-lock function.
type WithMutexLock struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WithMutexLock) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	m, ok := args[0].(*Mutex)
	if !ok {
		slip.TypePanic(s, depth, "mutex", args[0], "mutex")
	}
	defer (*sync.Mutex)(m).Unlock()
	(*sync.Mutex)(m).Lock()

	d2 := depth + 1
	forms := args[1:]
	for i := range forms {
		result = slip.EvalArg(s, forms, i, d2)
	}
	return
}
