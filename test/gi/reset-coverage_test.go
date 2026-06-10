// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestResetCoverageFile(t *testing.T) {
	defer func() {
		slip.StopCoverage()
	}()
	(&sliptest.Function{
		Source: `(let (reports)
                   (setq *coverage* t)
                   (load "testdata/coverage/sample.lisp")
                   (addf reports (coverage-report nil))
                   (reset-coverage)
                   (addf reports (coverage-report nil))
                   (reset-coverage t)
                   (addf reports (coverage-report nil))
                   reports)`,
		Validate: func(t *testing.T, v slip.Object) {
			reports, _ := v.(slip.List)
			tt.Equal(t, 3, len(reports))
			tt.Equal(t, `/coverage\/sample.lisp.*1 1 2 11 1\)/`, string(reports[0].(slip.String)))
			tt.Equal(t, `/coverage\/sample.lisp.*1 1 2 11 0\)/`, string(reports[1].(slip.String)))
			tt.Equal(t, "(())\n", string(reports[2].(slip.String)))
		},
	}).Test(t)
}
