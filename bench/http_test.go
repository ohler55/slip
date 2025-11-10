package bench_test

import (
	"fmt"
	"net"
	"net/http"
	"testing"
	"time"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg"
)

// go test -bench HTTP

func availablePort() int {
	addr, err := net.ResolveTCPAddr("tcp", "localhost:0")
	if err != nil {
		panic(err)
	}
	var listener *net.TCPListener
	if listener, err = net.ListenTCP("tcp", addr); err != nil {
		panic(err)
	}
	defer func() { _ = listener.Close() }()

	return listener.Addr().(*net.TCPAddr).Port
}

func BenchmarkHTTPSlip(b *testing.B) {
	port := availablePort()
	hs := http.Server{
		Addr: fmt.Sprintf(":%d", port),
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			_, _ = fmt.Fprintln(w, "Got it!")
		}),
	}
	go func() { _ = hs.ListenAndServe() }()
	defer func() { _ = hs.Close() }()

	su := fmt.Sprintf("http://localhost:%d", port)
	start := time.Now()
	for time.Since(start) < time.Second*2 {
		time.Sleep(time.Millisecond * 50)
		if resp, err := http.Get(su); err == nil {
			_ = resp.Body.Close()
			break
		}
	}
	scope := slip.NewScope()
	scope.Let("url", slip.String(su))
	_ = slip.ReadString(`(setq client (make-instance 'http-client-flavor :url url))`, scope).Eval(scope, nil)
	send := slip.ReadString(`(send (send client :get) :close)`, scope)

	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		send.Eval(scope, nil)
	}
}

func BenchmarkHTTPGo(b *testing.B) {
	port := availablePort()
	hs := http.Server{
		Addr: fmt.Sprintf(":%d", port),
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			_, _ = fmt.Fprintln(w, "Got it!")
		}),
	}
	go func() { _ = hs.ListenAndServe() }()
	defer func() { _ = hs.Close() }()

	su := fmt.Sprintf("http://localhost:%d", port)
	start := time.Now()
	for time.Since(start) < time.Second*2 {
		time.Sleep(time.Millisecond * 50)
		if resp, err := http.Get(su); err == nil {
			_ = resp.Body.Close()
			break
		}
	}

	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		res, _ := http.Get(su)
		_ = res.Body.Close()
	}
}
