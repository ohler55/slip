// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestUUID(t *testing.T) {
	u := gi.UUID{0x6ef16994701d44d5, 0x87ec7ef3e2e5709b}
	(&sliptest.Object{
		Target:    u,
		String:    "#<uuid 6ef16994-701d-44d5-87ec-7ef3e2e5709b>",
		Simple:    "6ef16994-701d-44d5-87ec-7ef3e2e5709b",
		Hierarchy: "uuid.t",
		Equals: []*sliptest.EqTest{
			{Other: u, Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
		},
		Eval: u,
	}).Test(t)
	tt.Equal(t, "6ef16994-701d-44d5-87ec-7ef3e2e5709b", u.IETF())
}

func TestUUIDParse(t *testing.T) {
	u := gi.UUIDParse("6ef16994-701d-44d5-87ec-7ef3e2e5709b")
	tt.Equal(t, "6ef16994-701d-44d5-87ec-7ef3e2e5709b", u.IETF())
	tt.Equal(t, false, u.IsNil())

	u = gi.UUIDParse("zzzz6994-701d-44d5-87ec-7ef3e2e5709b")
	tt.Equal(t, true, u.IsNil())
}

func TestUUIDNew(t *testing.T) {
	u := gi.NewUUID()
	tt.Equal(t, "/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89abc][0-9a-f]{3}-[0-9a-f]{12}$/", u.IETF())
}

func TestUUIDBytes(t *testing.T) {
	u := gi.UUIDParse("6ef16994-701d-44d5-87ec-7ef3e2e5709b")
	tt.Equal(t, []byte{
		0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
	}, u.Bytes())
}
