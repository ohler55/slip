// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/parquet"
)

func TestJSONToLisp(t *testing.T) {
	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(slip.Fixnum(3)))

	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(3))
	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(int8(3)))
	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(int16(3)))
	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(int32(3)))
	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(int64(3)))

	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(uint(3)))
	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(uint8(3)))
	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(uint16(3)))
	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(uint32(3)))
	tt.Equal(t, slip.Fixnum(3), parquet.JSONToLisp(uint64(3)))

	tt.Equal(t, slip.SingleFloat(2.5), parquet.JSONToLisp(float32(2.5)))
	tt.Equal(t, slip.DoubleFloat(2.5), parquet.JSONToLisp(float64(2.5)))

	tt.Equal(t, slip.String("abc"), parquet.JSONToLisp("abc"))
	tt.Equal(t, slip.String("abc"), parquet.JSONToLisp([]byte("abc")))

	tm := time.Now()
	tt.Equal(t, slip.Time(tm), parquet.JSONToLisp(tm))
}
