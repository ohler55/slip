// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"fmt"
	"io"

	"github.com/ohler55/slip"
)

var (
	// Pkg is the watch package.
	Pkg = slip.Package{
		Name:      "watch",
		Nicknames: []string{},
		Doc: `The _watch_ package facilitates inspection and evaluation on a remote SLIP
process. A _watch-server_ is set up in the server process and one or more
clients can then connect with the server and make a request to evaluate an
expression, watch glbal variables, or periodically evaluate and expression and
return the results.


__Protocol__


The client and servers exchange LISP _list_ expressions. The first element of
the lists exchanges must be a _symbol_ that indicates either the request or
response type. The protocol is only described here to allow non-SLIP
application to interact with a _watch-server_ otherwise the exchanges are
initiated using client methods. Requests types sent to the server are
explained by examples:

 __(eval 7 '(+ quux 3))__
 An _eval_ is a request to evaluate an expression. The second element of the
list is a _fixnum_ identifier for the request. The third element is the
expression to evaluate.

 __(watch *some-var*)__
 A request to report any change in the specified variable which in this
example is _*some-var*_.

 __(periodic p5 5.5 (lambda () (now)))__
 A request to set up a periodic evaluation where the result is sent back to
the client. The second element of the request list is an identifier for the
periodic being established and must be a symbol. The third element must be a
_real_ and is the number of seconds for the interval between evaluations. The
interval is rounded to 100 milliseconds. The last element of the list is the
expression to evaluate which can be a _symbol_ to return the value of or a
_lambda_ expression.

 __(forget id)__
 A request to forget or stop watching a variable or to cancel a periodic evaluation.


Responses types to the client are:

 __(result 7 9)__
 A _result_ expression is sent as a response to an _eval_ request and consists
of the _result_ symbol followed by the _eval_request identifier and the result
of the evaluation.

 __(changed *some-var* "abc")__

 A _changed_ response is sent when a watched variable changes. The response
expression is the _changed_ symbol followed by the variable that has changed
and the new value. The _changed_ response is also send to deliver the result
of a periodic evaluation. In that case the second element is the identifier
provided when the periodic was created.

 __(error id type-error "something about the error")__
 An _error_ response is sent when and evaluation initiated by an _eval_ or a
_periodic_ fails. The second element is the identifier for the evaluation or
nil if the identifier can not be determined. The third element is the type of
error and the last element is the error message.


__Use__


To use the watch functionality a _watch-server_ is started on the server SLIP
instance with a given port number. One or more client can then be started in a
different SLIP instance and the host and port of the server provided on client
creation.


Several client variants are provided in the _watch_ package. All support the
_:eval_, _:watch_, _:periodic_, _:forget_, and _:shutdown_ methods that make
requests to the server. In addition all clients respond to replies that call
the _:changed_ method. Each variant is a sub-class or sub-flavor of the
_watch-client_. Custom clients can be defined using the same sub-classing
approach.

 ;; server side
 (defvar ws (make-instance 'watch-server :port 9999))

 ;; client side
 (setq wc (make-instance 'watch-framer
                         :host "127.0.0.1"
                         :port 9999
                         :top 2
                         :left 3
                         :border :line
                         :watch '(foo bar quux)
                         :periodics '((p4 4 (lambda () (now))))))


The package provided client variants are:

 __watch-printer__
 A client that prints all _:changed_ calls to _*standard-output*_. This allows
for capturing a log of changes.

 __watch-channeler__
 A client that places changes on a _channel_ so that another thread can process the changes.

 __watch-framer__
 A client that displays changes in a frame using ANSI codes. This is
particularly useful when viewing just the current values of a variable or
periodic result. A position of the the top left corner is specified along with
what type if any border should be drawn around the symbols and values.


__Examples__


__:eval__

The _watch-client_, the base flavor for other clients includes the _:eval_
method. The _:eval_ method blocks until a response is received or the provided
timeout is reached.

 (let ((wc (make-instance 'watch-client :host "127.0.0.1" :port 9999)))
  (send wc :eval '(setq quux 6))
  ;; returns 6
  (send wc :eval '(+ quux 3)))
  ;; returns 9


__watch-printer_

The _watch-printer_ adds an _:after_ daemon to the client _:changed_ method
that prints the the changes to _*standard-output*_. That makes it useful for
logging changes. As might be expected, with asynchronous output it is usually
more useful to start up a _watch-printer_ in a non-interactive mode. One way
to do that is to create a file such as watch-this.lisp with the contents that
creates a watcher.

 (make-instance 'watch-printer :host "127.0.0.1" :port 9999 :watch '(quux))
 (signal-wait sigint sigterm)


Then run by forcing the slip application to not be interactive. Changes in the
variable quux on the server will then output a line on the client

 > go run cmd/slip/main.go -e " " watch-this.lisp


__watch-channeler__

The _watch-channeler_ adds an _:after_ daemon to the client _:changed_ method
that pushes changes onto a channel. An example use case might be to start a
_watch-server_ on multiple slip application and then have another slip app,
the client, create a _watch-client_ for each server. As responses to periodic
evaluations are received they could be pushed to a _channel_ that would then
be processed by a worker thread that writes to a log file.

 (let* ((chan (make-channel 10))
        (wc (make-instance 'watch-channeler
                           :host "127.0.0.1"
                           :port 9999
                           :periodic '((progress 10 *fast-counter*))
                           :channel chan)))
   (do ((change (channel-pop chan) (channel-pop chan)))
       ((null change) nil)
     (format t "changed: ~A~%" change)))


__watch-framer__

The _watch-framer_ is intended for monitoring and debugging. It displays
changes in a fixed location and values are updated in place.

 (setq wc (make-instance 'watch-framer
                         :host "127.0.0.1"
                         :port 9999
                         :top 2
                         :left 3
                         :border :line
                         :watch '(foo bar quux)
                         :periodics '((p4 4 (lambda () (now))))))
`,
		PreSet: slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*watch*": {Val: &Pkg, Doc: Pkg.Doc, Export: true},
		},
	)
	_ = ServerFlavor()
	_ = ClientFlavor()
	_ = ChannelerFlavor()
	_ = FramerFlavor()
	_ = PrinterFlavor()

	Pkg.Initialize(nil, &periodic{})

	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}

func displayError(format string, args ...any) {
	eo := slip.NewScope().Get("*error-output*").(io.Writer)
	fmt.Fprintf(eo, "\n*-*-* %s\n", fmt.Sprintf(format, args...))
}
