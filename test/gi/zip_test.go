// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"bytes"
	"compress/gzip"
	"encoding/base64"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

// With the changes in Go 1.27, the zip output has also changed. It's still
// valid zip but possibly a different seed is being used. To verify values are
// encoded correctly an unzip is performed and the unzip content is compared
// to the original.

func checkZipB64(t *testing.T, v slip.Object, comment, content string) *gzip.Reader {
	ss, _ := v.(slip.String)
	zb, err := base64.StdEncoding.DecodeString(string(ss))
	tt.Nil(t, err)
	br := bytes.NewReader(zb)
	var zr *gzip.Reader
	zr, err = gzip.NewReader(br)
	tt.Nil(t, err)

	tt.Equal(t, comment, zr.Comment)

	buf := make([]byte, 100)
	var n int
	n, err = zr.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, content, string(buf[:n]))

	return zr
}

func TestZip5(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (zip "some data" 5))`,
		Validate: func(t *testing.T, v slip.Object) {
			checkZipB64(t, v, "", "some data")
		},
	}).Test(t)
}

func TestZipDefault(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (zip "some data" nil))`,
		Validate: func(t *testing.T, v slip.Object) {
			checkZipB64(t, v, "", "some data")
		},
	}).Test(t)
}

func TestZipHeader(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (zip "some data" 5 :comment "test" :extra "xxx" :name "namai" :os 7))`,
		Validate: func(t *testing.T, v slip.Object) {
			zr := checkZipB64(t, v, "test", "some data")
			tt.Equal(t, "xxx", string(zr.Extra))
			tt.Equal(t, "namai", zr.Name)
			tt.Equal(t, 7, zr.OS)
		},
	}).Test(t)
}

func TestZipModTime(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (zip "some data" 9 :mod-time @2025-05-23T12:13:14Z))`,
		Expect: `"H4sIAFpmMGgC/yrOz01VSEksSQQAAAD//wEAAP//HunC2QkAAAA="`,
		Validate: func(t *testing.T, v slip.Object) {
			zr := checkZipB64(t, v, "", "some data")
			tt.Equal(t, "2025-05-23T12:13:14Z", zr.ModTime.UTC().Format(time.RFC3339))
		},
	}).Test(t)
}

func TestZipBadLevel(t *testing.T) {
	(&sliptest.Function{
		Source:    `(zip "some data" t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestZipBadTime(t *testing.T) {
	(&sliptest.Function{
		Source:    `(zip "some data" :mod-time t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestZipNewWriterError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(zip "some data" 100)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestZipError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(zip "some data" :comment "ぴ")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
