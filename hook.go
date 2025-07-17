// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slip

var (
	setHooks   []*hook
	unsetHooks []*hook
	defunHooks []*hook
	classHooks []*hook
)

type hook struct {
	fun func(p *Package, key string)
	id  string
}

// AddSetHook adds a hook that is called after setting a variable or after a
// defvar or defparameter is called.
func AddSetHook(id string, fun func(p *Package, key string)) {
	setHooks = append(setHooks, &hook{id: id, fun: fun})
}

// RemoveSetHook removes the set hook with the specified ID.
func RemoveSetHook(id string) {
	for i, h := range setHooks {
		if id == h.id {
			setHooks = append(setHooks[:i], setHooks[i+1:]...)
			return
		}
	}
}

// AddUnsetHook add a hook that is called when a variable is unset or removed.
func AddUnsetHook(id string, fun func(p *Package, key string)) {
	unsetHooks = append(unsetHooks, &hook{id: id, fun: fun})
}

// RemoveUnsetHook removes the unset hook with the specified ID.
func RemoveUnsetHook(id string) {
	for i, h := range unsetHooks {
		if id == h.id {
			unsetHooks = append(unsetHooks[:i], unsetHooks[i+1:]...)
			return
		}
	}
}

// AddDefunHook add a hook that is called after a function is added.
func AddDefunHook(id string, fun func(p *Package, key string)) {
	defunHooks = append(defunHooks, &hook{id: id, fun: fun})
}

// AddClassHook add a hook that is called after a class is added.
func AddClassHook(id string, fun func(p *Package, key string)) {
	classHooks = append(classHooks, &hook{id: id, fun: fun})
}

// RemoveDefunHook removes the defun hook with the specified ID.
func RemoveDefunHook(id string) {
	for i, h := range defunHooks {
		if id == h.id {
			defunHooks = append(defunHooks[:i], defunHooks[i+1:]...)
			return
		}
	}
}

// RemoveClassHook removes the class hook with the specified ID.
func RemoveClassHook(id string) {
	for i, h := range classHooks {
		if id == h.id {
			classHooks = append(classHooks[:i], classHooks[i+1:]...)
			return
		}
	}
}

func callSetHooks(pkg *Package, name string) {
	for _, h := range setHooks {
		h.fun(pkg, name)
	}
}
