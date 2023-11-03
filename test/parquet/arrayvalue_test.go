// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"
	"time"

	"github.com/apache/arrow/go/v14/arrow"
	"github.com/apache/arrow/go/v14/arrow/array"
	"github.com/apache/arrow/go/v14/arrow/float16"
	"github.com/apache/arrow/go/v14/arrow/memory"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/parquet"
)

func TestArrayValueNull(t *testing.T) {
	aa := array.NewNull(2)
	tt.Equal(t, nil, parquet.ArrayValue(aa, 0))
}

func TestArrayValueBoolean(t *testing.T) {
	b := array.NewBooleanBuilder(memory.DefaultAllocator)
	b.AppendValues([]bool{true, false}, nil)
	b.AppendNull()
	aa := b.NewBooleanArray()
	tt.Equal(t, slip.True, parquet.ArrayValue(aa, 0))
	tt.Equal(t, nil, parquet.ArrayValue(aa, 1))
	tt.Equal(t, nil, parquet.ArrayValue(aa, 2))
}

func TestArrayValueInt8(t *testing.T) {
	b := array.NewInt8Builder(memory.DefaultAllocator)
	b.AppendValues([]int8{1, 2}, nil)
	b.AppendNull()
	aa := b.NewInt8Array()
	tt.Equal(t, slip.Fixnum(1), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Fixnum(2), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Fixnum(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueInt16(t *testing.T) {
	b := array.NewInt16Builder(memory.DefaultAllocator)
	b.AppendValues([]int16{1, 2}, nil)
	b.AppendNull()
	aa := b.NewInt16Array()
	tt.Equal(t, slip.Fixnum(1), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Fixnum(2), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Fixnum(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueInt32(t *testing.T) {
	b := array.NewInt32Builder(memory.DefaultAllocator)
	b.AppendValues([]int32{1, 2}, nil)
	b.AppendNull()
	aa := b.NewInt32Array()
	tt.Equal(t, slip.Fixnum(1), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Fixnum(2), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Fixnum(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueInt64(t *testing.T) {
	b := array.NewInt64Builder(memory.DefaultAllocator)
	b.AppendValues([]int64{1, 2}, nil)
	b.AppendNull()
	aa := b.NewInt64Array()
	tt.Equal(t, slip.Fixnum(1), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Fixnum(2), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Fixnum(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueUint8(t *testing.T) {
	b := array.NewUint8Builder(memory.DefaultAllocator)
	b.AppendValues([]uint8{1, 2}, nil)
	b.AppendNull()
	aa := b.NewUint8Array()
	tt.Equal(t, slip.Fixnum(1), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Fixnum(2), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Fixnum(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueUint16(t *testing.T) {
	b := array.NewUint16Builder(memory.DefaultAllocator)
	b.AppendValues([]uint16{1, 2}, nil)
	b.AppendNull()
	aa := b.NewUint16Array()
	tt.Equal(t, slip.Fixnum(1), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Fixnum(2), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Fixnum(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueUint32(t *testing.T) {
	b := array.NewUint32Builder(memory.DefaultAllocator)
	b.AppendValues([]uint32{1, 2}, nil)
	b.AppendNull()
	aa := b.NewUint32Array()
	tt.Equal(t, slip.Fixnum(1), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Fixnum(2), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Fixnum(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueUint64(t *testing.T) {
	b := array.NewUint64Builder(memory.DefaultAllocator)
	b.AppendValues([]uint64{1, 2}, nil)
	b.AppendNull()
	aa := b.NewUint64Array()
	tt.Equal(t, slip.Fixnum(1), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Fixnum(2), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Fixnum(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueFloat16(t *testing.T) {
	b := array.NewFloat16Builder(memory.DefaultAllocator)
	b.AppendValues([]float16.Num{float16.New(1.5), float16.New(2.5)}, nil)
	b.AppendNull()
	aa := b.NewFloat16Array()
	tt.Equal(t, slip.SingleFloat(1.5), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.SingleFloat(2.5), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.SingleFloat(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueFloat32(t *testing.T) {
	b := array.NewFloat32Builder(memory.DefaultAllocator)
	b.AppendValues([]float32{1.5, 2.5}, nil)
	b.AppendNull()
	aa := b.NewFloat32Array()
	tt.Equal(t, slip.SingleFloat(1.5), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.SingleFloat(2.5), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.SingleFloat(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueFloat64(t *testing.T) {
	b := array.NewFloat64Builder(memory.DefaultAllocator)
	b.AppendValues([]float64{1.5, 2.5}, nil)
	b.AppendNull()
	aa := b.NewFloat64Array()
	tt.Equal(t, slip.DoubleFloat(1.5), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.DoubleFloat(2.5), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.DoubleFloat(0), parquet.ArrayValue(aa, 2))
}

func TestArrayValueString(t *testing.T) {
	b := array.NewStringBuilder(memory.DefaultAllocator)
	b.AppendValues([]string{"one", "two"}, nil)
	b.AppendNull()
	aa := b.NewStringArray()
	tt.Equal(t, slip.String("one"), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.String("two"), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.String(""), parquet.ArrayValue(aa, 2))
}

func TestArrayValueBinary(t *testing.T) {
	b := array.NewBinaryBuilder(memory.DefaultAllocator, &arrow.BinaryType{})
	b.AppendStringValues([]string{"one", "two"}, nil)
	b.AppendNull()
	aa := b.NewBinaryArray()
	tt.Equal(t, slip.String("one"), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.String("two"), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.String(""), parquet.ArrayValue(aa, 2))
}

func TestArrayValueFixedSizeBinary(t *testing.T) {
	b := array.NewFixedSizeBinaryBuilder(memory.DefaultAllocator, &arrow.FixedSizeBinaryType{ByteWidth: 3})
	b.AppendValues([][]byte{[]byte("one"), []byte("two")}, nil)
	b.AppendNull()
	aa := b.NewFixedSizeBinaryArray()
	tt.Equal(t, slip.String("one"), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.String("two"), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.String("\x00\x00\x00"), parquet.ArrayValue(aa, 2))
}

func TestArrayValueDate32(t *testing.T) {
	b := array.NewDate32Builder(memory.DefaultAllocator)
	b.AppendValues([]arrow.Date32{19661, 19672}, nil)
	b.AppendNull()
	aa := b.NewDate32Array()
	today, _ := time.Parse(time.RFC3339, "2023-10-31T00:00:00Z")
	another, _ := time.Parse(time.RFC3339, "2023-11-11T00:00:00Z")
	null, _ := time.Parse(time.RFC3339, "1970-01-01T00:00:00Z")

	tt.Equal(t, slip.Time(today), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Time(another), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Time(null), parquet.ArrayValue(aa, 2))
}

func TestArrayValueDate64(t *testing.T) {
	b := array.NewDate64Builder(memory.DefaultAllocator)
	b.AppendValues([]arrow.Date64{1698750000000, 1699700000000}, nil)
	b.AppendNull()
	aa := b.NewDate64Array()
	today, _ := time.Parse(time.RFC3339, "2023-10-31T00:00:00Z")
	another, _ := time.Parse(time.RFC3339, "2023-11-11T00:00:00Z")
	null, _ := time.Parse(time.RFC3339, "1970-01-01T00:00:00Z")

	tt.Equal(t, slip.Time(today), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Time(another), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Time(null), parquet.ArrayValue(aa, 2))
}

func TestArrayValueTime32(t *testing.T) {
	b := array.NewTime32Builder(memory.DefaultAllocator, &arrow.Time32Type{Unit: arrow.Second})
	b.AppendValues([]arrow.Time32{1698714123, 1699675506}, nil)
	b.AppendNull()
	aa := b.NewTime32Array()
	today, _ := time.Parse(time.RFC3339, "2023-10-31T01:02:03Z")
	another, _ := time.Parse(time.RFC3339, "2023-11-11T04:05:06Z")
	null, _ := time.Parse(time.RFC3339, "1970-01-01T00:00:00Z")
	tt.Equal(t, slip.Time(today), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Time(another), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Time(null), parquet.ArrayValue(aa, 2))
}

func TestArrayValueTime64(t *testing.T) {
	b := array.NewTime64Builder(memory.DefaultAllocator, &arrow.Time64Type{Unit: arrow.Second})
	b.AppendValues([]arrow.Time64{1698714123, 1699675506}, nil)
	b.AppendNull()
	aa := b.NewTime64Array()
	today, _ := time.Parse(time.RFC3339, "2023-10-31T01:02:03Z")
	another, _ := time.Parse(time.RFC3339, "2023-11-11T04:05:06Z")
	null, _ := time.Parse(time.RFC3339, "1970-01-01T00:00:00Z")
	tt.Equal(t, slip.Time(today), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Time(another), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Time(null), parquet.ArrayValue(aa, 2))
}

func TestArrayValueDuration(t *testing.T) {
	b := array.NewDurationBuilder(memory.DefaultAllocator, &arrow.DurationType{Unit: arrow.Second})
	b.AppendValues([]arrow.Duration{100, 200}, nil)
	b.AppendNull()
	aa := b.NewDurationArray()
	tt.Equal(t, slip.Fixnum(100), parquet.ArrayValue(aa, 0))
	tt.Equal(t, slip.Fixnum(200), parquet.ArrayValue(aa, 1))
	tt.Equal(t, slip.Fixnum(0), parquet.ArrayValue(aa, 2))
}
