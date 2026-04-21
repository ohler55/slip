package bench_test

import "testing"

type benchScope struct {
	InterruptCheck func()
}

func BenchmarkInterruptNilCheck(b *testing.B) {
	s := &benchScope{}
	for i := 0; i < b.N; i++ {
		if s.InterruptCheck != nil {
			s.InterruptCheck()
		}
	}
}

func BenchmarkInterruptEmptyCall(b *testing.B) {
	s := &benchScope{InterruptCheck: func() {}}
	for i := 0; i < b.N; i++ {
		s.InterruptCheck()
	}
}
