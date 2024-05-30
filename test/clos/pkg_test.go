// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/pkg/clos"
)

func TestMethodDocFromFunc(t *testing.T) {
	tt.Equal(t, `__quux__ _function_ &optional _path_ _as-bag_ => _bag_
   _function_ [function] The function to apply to each node in bag matching the _path_.
   _path_ [string|bag-path] The path to the location in the bag to walk.
The path must follow the JSONPath format. Default: ".."
   _as-bag_ [boolean] If not nil then the value to the _function_ is a _bag_ value otherwise a new LISP object.


The __quux__ method walks the values at the location described by _path_.

This is the same as the _:walk_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.


Examples:
  (setq bag (make-instance 'bag-flavor :parse "{a:7 b:8}") result '())
  (bag-walk bag (lambda (x) (setq result cons x result)) "*") => nil
  result => (7 8)


See also: __bag-walk__
`, clos.MethodDocFromFunc("quux", "bag-walk", "bag", "bg"))
}
