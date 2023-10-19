// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReaderBasic(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/sample.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `reader`,
		Expect: "/^#<parquet-reader-flavor [0-9a-f]+>$/",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :filepath)`,
		Expect: `"testdata/sample.parquet"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :version)`,
		Expect: `"v1.0"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :created-by)`,
		Expect: `"parquet-mr version 1.10.1 (build 7d648c1076647085126b685ba8288c8b8bf719ce)"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :row-count)`,
		Expect: "5",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :column-count)`,
		Expect: "45",
	}).Test(t)

	testSchema(t, scope, slip.ReadString(`(send reader :schema)`).Eval(scope, nil).(slip.List))
}

func testSchema(t *testing.T, scope *slip.Scope, schema slip.List) {
	var b []byte
	// Verify schema elements. For element the target method ouput along with
	// the name is collected and placed in a bufffer. That buffer is then
	// checked.
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = append(b, slip.ReadString(`(send element :write nil)`).Eval(scope, nil).(slip.String)...)
	}
	tt.Equal(t, `  optional int32 event_date (DATE);
  optional byte_array request_id (UTF8);
  optional byte_array upload_id (UTF8);
  optional byte_array subject_id (UTF8);
  optional int96 received_time;
  optional byte_array sample_type (UTF8);
  optional byte_array sample_id (UTF8);
  optional int96 creation_time;
  optional int96 start_time;
  optional int96 end_time;
  optional byte_array name (UTF8);
  optional byte_array revision_product_type (UTF8);
  optional byte_array bundle_id (UTF8);
  optional int64 options;
  optional byte_array version (UTF8);
  optional byte_array product_type (UTF8);
  optional byte_array os_version (UTF8);
  optional byte_array name (UTF8);
  optional byte_array manufacturer (UTF8);
  optional byte_array model (UTF8);
  optional byte_array hardware_version (UTF8);
  optional byte_array firmware_version (UTF8);
  optional byte_array software_version (UTF8);
  optional byte_array local_id (UTF8);
  optional byte_array udi_id (UTF8);
  required byte_array key (UTF8);
  required byte_array string_value (UTF8);
  required int96 time_value;
  required int64 integer_value;
  required double double_value;
  required double original_value;
  required byte_array original_unit (UTF8);
  required byte_array data_value;
  optional int64 start_sequence;
  optional int64 end_sequence;
  optional int96 start_time;
  optional int96 end_time;
  optional int96 creation_time;
  required byte_array lead_name (UTF8);
  repeated double element;
  optional int64 count;
  optional double frequency;
  optional double avg_heart_rate;
  optional byte_array classification (UTF8);
  optional int64 symptoms;
`, string(b))

	// verify :type
	b = b[:0]
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = append(b, slip.ReadString(`(send element :name)`).Eval(scope, nil).(slip.String)...)
		b = append(b, ' ')
		b = appendValue(b, slip.ReadString(`(send element :type)`).Eval(scope, nil))
		b = append(b, '\n')
	}
	tt.Equal(t, `event_date INT32
request_id BYTE_ARRAY
upload_id BYTE_ARRAY
subject_id BYTE_ARRAY
received_time INT96
sample_type BYTE_ARRAY
sample_id BYTE_ARRAY
creation_time INT96
start_time INT96
end_time INT96
name BYTE_ARRAY
revision_product_type BYTE_ARRAY
bundle_id BYTE_ARRAY
options INT64
version BYTE_ARRAY
product_type BYTE_ARRAY
os_version BYTE_ARRAY
name BYTE_ARRAY
manufacturer BYTE_ARRAY
model BYTE_ARRAY
hardware_version BYTE_ARRAY
firmware_version BYTE_ARRAY
software_version BYTE_ARRAY
local_id BYTE_ARRAY
udi_id BYTE_ARRAY
key BYTE_ARRAY
string_value BYTE_ARRAY
time_value INT96
integer_value INT64
double_value DOUBLE
original_value DOUBLE
original_unit BYTE_ARRAY
data_value BYTE_ARRAY
start_sequence INT64
end_sequence INT64
start_time INT96
end_time INT96
creation_time INT96
lead_name BYTE_ARRAY
element DOUBLE
count INT64
frequency DOUBLE
avg_heart_rate DOUBLE
classification BYTE_ARRAY
symptoms INT64
`, string(b))

	// verify path
	b = b[:0]
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = append(b, slip.ReadString(`(send element :name)`).Eval(scope, nil).(slip.String)...)
		b = append(b, ' ')
		b = appendValue(b, slip.ReadString(`(send element :logical-type)`).Eval(scope, nil))
		b = append(b, '\n')
	}
	tt.Equal(t, `event_date Date
request_id String
upload_id String
subject_id String
received_time nil
sample_type String
sample_id String
creation_time nil
start_time nil
end_time nil
name String
revision_product_type String
bundle_id String
options nil
version String
product_type String
os_version String
name String
manufacturer String
model String
hardware_version String
firmware_version String
software_version String
local_id String
udi_id String
key String
string_value String
time_value nil
integer_value nil
double_value nil
original_value nil
original_unit String
data_value nil
start_sequence nil
end_sequence nil
start_time nil
end_time nil
creation_time nil
lead_name String
element nil
count nil
frequency nil
avg_heart_rate nil
classification String
symptoms nil
`, string(b))

	// verify converted-type
	b = b[:0]
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = append(b, slip.ReadString(`(send element :name)`).Eval(scope, nil).(slip.String)...)
		b = append(b, ' ')
		b = appendValue(b, slip.ReadString(`(send element :converted-type)`).Eval(scope, nil))
		b = append(b, '\n')
	}
	tt.Equal(t, `event_date DATE
request_id UTF8
upload_id UTF8
subject_id UTF8
received_time nil
sample_type UTF8
sample_id UTF8
creation_time nil
start_time nil
end_time nil
name UTF8
revision_product_type UTF8
bundle_id UTF8
options nil
version UTF8
product_type UTF8
os_version UTF8
name UTF8
manufacturer UTF8
model UTF8
hardware_version UTF8
firmware_version UTF8
software_version UTF8
local_id UTF8
udi_id UTF8
key UTF8
string_value UTF8
time_value nil
integer_value nil
double_value nil
original_value nil
original_unit UTF8
data_value nil
start_sequence nil
end_sequence nil
start_time nil
end_time nil
creation_time nil
lead_name UTF8
element nil
count nil
frequency nil
avg_heart_rate nil
classification UTF8
symptoms nil
`, string(b))

	// verify :path
	b = b[:0]
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = append(b, slip.ReadString(`(send element :name)`).Eval(scope, nil).(slip.String)...)
		b = append(b, ' ')
		b = appendValue(b, slip.ReadString(`(send element :path)`).Eval(scope, nil))
		b = append(b, '\n')
	}
	tt.Equal(t, `event_date event_date
request_id request_id
upload_id upload_id
subject_id subject_id
received_time received_time
sample_type sample_type
sample_id sample_id
creation_time creation_time
start_time start_time
end_time end_time
name source.name
revision_product_type source.revision_product_type
bundle_id source.bundle_id
options source.options
version source.version
product_type source.product_type
os_version source.os_version
name device.name
manufacturer device.manufacturer
model device.model
hardware_version device.hardware_version
firmware_version device.firmware_version
software_version device.software_version
local_id device.local_id
udi_id device.udi_id
key metadata.kv.key_value.key
string_value metadata.kv.key_value.value.string_value
time_value metadata.kv.key_value.value.time_value
integer_value metadata.kv.key_value.value.integer_value
double_value metadata.kv.key_value.value.double_value
original_value metadata.kv.key_value.value.quantity_value.original_value
original_unit metadata.kv.key_value.value.quantity_value.original_unit
data_value metadata.kv.key_value.value.data_value
start_sequence query.start_sequence
end_sequence query.end_sequence
start_time query.start_time
end_time query.end_time
creation_time query.creation_time
lead_name leads.list.element.lead_name
element leads.list.element.value_microvolts.list.element
count count
frequency frequency
avg_heart_rate avg_heart_rate
classification classification
symptoms symptoms
`, string(b))

	// verify :path
	b = b[:0]
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = append(b, slip.ReadString(`(send element :name)`).Eval(scope, nil).(slip.String)...)
		b = append(b, ' ')
		b = appendValue(b, slip.ReadString(`(send element :type-length)`).Eval(scope, nil))
		b = append(b, '\n')
	}
	tt.Equal(t, `event_date nil
request_id nil
upload_id nil
subject_id nil
received_time nil
sample_type nil
sample_id nil
creation_time nil
start_time nil
end_time nil
name nil
revision_product_type nil
bundle_id nil
options nil
version nil
product_type nil
os_version nil
name nil
manufacturer nil
model nil
hardware_version nil
firmware_version nil
software_version nil
local_id nil
udi_id nil
key nil
string_value nil
time_value nil
integer_value nil
double_value nil
original_value nil
original_unit nil
data_value nil
start_sequence nil
end_sequence nil
start_time nil
end_time nil
creation_time nil
lead_name nil
element nil
count nil
frequency nil
avg_heart_rate nil
classification nil
symptoms nil
`, string(b))

	// verify :repetiion
	b = b[:0]
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = append(b, slip.ReadString(`(send element :name)`).Eval(scope, nil).(slip.String)...)
		b = append(b, ' ')
		b = appendValue(b, slip.ReadString(`(send element :repetition)`).Eval(scope, nil))
		b = append(b, '\n')
	}
	tt.Equal(t, `event_date required
request_id required
upload_id required
subject_id required
received_time required
sample_type required
sample_id required
creation_time required
start_time required
end_time required
name required
revision_product_type required
bundle_id required
options required
version required
product_type required
os_version required
name required
manufacturer required
model required
hardware_version required
firmware_version required
software_version required
local_id required
udi_id required
key optional
string_value optional
time_value optional
integer_value optional
double_value optional
original_value optional
original_unit optional
data_value optional
start_sequence required
end_sequence required
start_time required
end_time required
creation_time required
lead_name optional
element repeated
count required
frequency required
avg_heart_rate required
classification required
symptoms required
`, string(b))

	// verify :max-definitions
	b = b[:0]
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = append(b, slip.ReadString(`(send element :name)`).Eval(scope, nil).(slip.String)...)
		b = append(b, ' ')
		b = appendValue(b, slip.ReadString(`(send element :max-definitions)`).Eval(scope, nil))
		b = append(b, '\n')
	}
	tt.Equal(t, `event_date 1
request_id 1
upload_id 1
subject_id 1
received_time 1
sample_type 1
sample_id 1
creation_time 1
start_time 1
end_time 1
name 2
revision_product_type 2
bundle_id 2
options 2
version 2
product_type 2
os_version 2
name 2
manufacturer 2
model 2
hardware_version 2
firmware_version 2
software_version 2
local_id 2
udi_id 2
key 3
string_value 5
time_value 5
integer_value 5
double_value 5
original_value 6
original_unit 6
data_value 5
start_sequence 2
end_sequence 2
start_time 2
end_time 2
creation_time 2
lead_name 4
element 5
count 1
frequency 1
avg_heart_rate 1
classification 1
symptoms 1
`, string(b))

}

/*
func testGroups(t *testing.T, scope *slip.Scope, groups slip.List) {
	tt.Equal(t, 1, len(groups))
	scope.Let(slip.Symbol("group"), groups[0])
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send group :size)`,
		Expect: "618938",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send group :row-count)`,
		Expect: "5",
	}).Test(t)
	tf := sliptest.Function{
		Scope:  scope,
		Source: `(send group :columns)`,
		Expect: "/<parquet-column-flavor/",
	}
	tf.Test(t)
	var b []byte
	for _, col := range tf.Result.(slip.List) {
		scope.Let(slip.Symbol("col"), col)
		typ := slip.ReadString(`(send col :type)`).Eval(scope, nil)
		b = appendValue(b, typ)
		b = append(b, '\n')
	}
	tt.Equal(t, `INT32
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
INT96
BYTE_ARRAY
BYTE_ARRAY
INT96
INT96
INT96
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
INT64
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
BYTE_ARRAY
INT96
INT64
DOUBLE
DOUBLE
BYTE_ARRAY
BYTE_ARRAY
INT64
INT64
INT96
INT96
INT96
BYTE_ARRAY
DOUBLE
INT64
DOUBLE
DOUBLE
BYTE_ARRAY
INT64
`, string(b))
	// TBD check values and walk
}
*/

func appendValue(b []byte, value slip.Object) []byte {
	switch tv := value.(type) {
	case slip.String:
		b = append(b, string(tv)...)
	default:
		b = slip.Append(b, tv)
	}
	return b
}

func TestReaderDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :init out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":init"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :close out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":close"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :version out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":version"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :created-by out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":created-by"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :row-count out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":row-count"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :column-count out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":column-count"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :schema out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":schema"))
}

func TestReaderBadInitFile(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance 'parquet-reader-flavor :file "no-file")`).Eval(scope, nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance 'parquet-reader-flavor :file t)`).Eval(scope, nil)
	})
}
