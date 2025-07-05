// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// Locker interface is used to act as an interface for either a sync.Mutex or
// a non-operational stand in for a mutex. Note that changing from one to the
// other should only be done while there is only one thread using the Locker.
type Locker interface {
	Lock()
	Unlock()
	TryLock() bool
}

// NoOpLocker is a Locker that does nothing acting instead like a Mutex that
// does nothing.
type NoOpLocker struct{}

// Lock does nothing.
func (NoOpLocker) Lock() {
}

// Unlock does nothing.
func (NoOpLocker) Unlock() {
}

// TryLock does nothing.
func (NoOpLocker) TryLock() bool {
	return false
}
