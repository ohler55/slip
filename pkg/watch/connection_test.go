// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"net"
	"sync"
	"testing"
)

func TestConnectionShutdownConcurrent(t *testing.T) {
	for i := 0; i < 100; i++ {
		con, peer := net.Pipe()
		c := newConnection(con)
		c.active.Store(true)

		panics := make(chan any, 8)
		var wg sync.WaitGroup

		for range 8 {
			wg.Add(1)
			go func() {
				defer wg.Done()
				defer func() {
					if rec := recover(); rec != nil {
						panics <- rec
					}
				}()
				c.shutdown(false)
			}()
		}
		wg.Wait()
		_ = peer.Close()
		close(panics)

		for p := range panics {
			t.Fatalf("shutdown panicked on iteration %d: %v", i, p)
		}
	}
}
