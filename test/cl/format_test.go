// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFormatStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(format out "abc")`, scope).Eval(scope, nil)

	tt.Equal(t, nil, result)
	tt.Equal(t, "abc", out.String())
}

func TestFormatString(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "abc")`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestFormatTrue(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(format t "abc")`,
		Expect: `nil`,
	}).Test(t)
	tt.Equal(t, `abc`, out.String())
}

func TestFormatWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(format out "abc")`,
		Panics: true,
	}).Test(t)
}

func TestFormatBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil)`,
		Panics: true,
	}).Test(t)
}

func TestFormatBadControl(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil nil)`,
		Panics: true,
	}).Test(t)
}

func TestFormatBadDestination(t *testing.T) {
	(&sliptest.Function{
		Source: `(format 'bad "abc")`,
		Panics: true,
	}).Test(t)
}

func TestFormatDoubleColon(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~::A")`,
		Panics: true,
	}).Test(t)
}

func TestFormatDoubleAt(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~@@A")`,
		Panics: true,
	}).Test(t)
}

func TestFormatBadParamOrder(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~@,1A")`,
		Panics: true,
	}).Test(t)
}

func TestFormatBadIntParam(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~1.3A")`,
		Panics: true,
	}).Test(t)
}

func TestFormatBadDir(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~Z")`,
		Panics: true,
	}).Test(t)
}

func TestFormatNewline(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "ab~
   cd")`,
		Expect: `"abcd"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "ab~:
   cd")`,
		Expect: `"ab   cd"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "ab~@
   cd")`,
		Expect: `"ab
cd"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "ab~:@
   cd")`,
		Panics: true,
	}).Test(t)
}

func TestFormatMoney(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~$" 1.234)`,
		Expect: `"1.23"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~3,2,7,'_$" 1.23456)`,
		Expect: `"_01.235"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,2,7,'_$" -1.23456)`,
		Expect: `"_-01.23"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,2,7,'_:@$" 1.23456)`,
		Expect: `"+_01.23"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,,7:@$" -1.23456)`,
		Expect: `"-  1.23"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~$" t)`,
		Panics: true,
	}).Test(t)
}

func TestFormatPercent(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~%")`,
		Expect: `"
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~3%")`,
		Expect: `"


"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v%" 2)`,
		Expect: `"

"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v%" t)`,
		Panics: true,
	}).Test(t)
}

func TestFormatAmp(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~&")`,
		Expect: `"
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~%~&")`,
		Expect: `"
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "
~2&")`,
		Expect: `"

"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v&" 2)`,
		Expect: `"

"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v&" t)`,
		Panics: true,
	}).Test(t)
}

func TestFormatCase(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~(Abc DEF Ghi~)")`,
		Expect: `"abc def ghi"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:(Abc DEF Ghi~)")`,
		Expect: `"Abc Def Ghi"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~@(Abc DEF Ghi~)")`,
		Expect: `"Abc def ghi"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~@(abc~)")`,
		Expect: `"Abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:@(Abc DEF Ghi~)")`,
		Expect: `"ABC DEF GHI"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~(abc ~:@(def~) Ghi~)")`,
		Expect: `"abc def ghi"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~(abc ~~ def~)")`,
		Expect: `"abc ~ def"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~(abc~:)")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~(abc~@)")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~(abc")`,
		Panics: true,
	}).Test(t)
}

func TestFormatMove(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~*~A" 1 2 3)`,
		Expect: `"2"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~2*~A" 1 2 3)`,
		Expect: `"3"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~A ~1:*~A" 1 2 3)`,
		Expect: `"1 1"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~1@*~A" 1 2 3)`,
		Expect: `"2"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v@*~A" 1 2 3)`,
		Expect: `"2"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~A ~@*~A" 1 2 3)`,
		Expect: `"1 1"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v@*~A" t 2 3)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:@*~A" 1 2 3)`,
		Panics: true,
	}).Test(t)
}

func TestFormatCall(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`(defun test-call-dir (s arg colon at) (format s "~A ~A ~A" arg colon at))`, scope)
	code.Eval(scope, nil)
	defer func() { slip.CurrentPackage = &slip.UserPkg }()
	slip.CurrentPackage.Export("test-call-dir")

	slip.CurrentPackage = slip.DefPackage("call-test", []string{}, "testing")
	slip.CurrentPackage.Use(&slip.UserPkg)

	code = slip.ReadString(`(defun test-pkg-call-dir (s arg colon at) (format s "~A ~A ~A" arg colon at))`, scope)
	code.Eval(scope, nil)

	(&sliptest.Function{
		Source: `(format nil "~/test-call-dir/" 3)`,
		Expect: `"3 nil nil"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:@/call-test::test-pkg-call-dir/" 3)`,
		Expect: `"3 t t"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~/test-call-dir" 3)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~/bad:test-call-dir/" 3)`,
		Panics: true,
	}).Test(t)
}

func TestFormatJustify(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~10,,1<abc~;def~>")`,
		Expect: `"abc    def"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~10,,1:@<abc~;def~>")`,
		Expect: `" abc def  "`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~10,3,1<abc~;def~;ghi~>")`,
		Expect: `"abc  def  ghi"`,
	}).Test(t)
	// nested
	(&sliptest.Function{
		Source: `(format nil "~10,,1<abc~;~4,,1,'z<d~;e~;f~>~>")`,
		Expect: `"abc  dzezf"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~10,,1<a~Dc~;def~>" 3)`,
		Expect: `"a3c    def"`,
	}).Test(t)
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*print-right-margin*"), slip.Fixnum(6))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(format nil "~,,1<abc~:;def~;ghi~>")`,
		Expect: `"abcdef ghi"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~5,,1<abc~^~;def~>")`,
		Expect: `"abc  "`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~8,,3:@<abc~;def~>")`,
		Expect: `"   abc   def   "`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~<abc~;def~:@>")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~<abc~;def")`,
		Panics: true,
	}).Test(t)
}

func TestFormatEval(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~=(+ 1 2)~=")`,
		Expect: `"3"`,
	}).Test(t)
	scope := slip.NewScope()
	scope.Let(slip.Symbol("value"), slip.Fixnum(3))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(format nil "~=value~=")`,
		Expect: `"3"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~=(format nil \"x~Ax\" 3)~=")`,
		Expect: `"x3x"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~=(+ 1 2)")`,
		Panics: true,
	}).Test(t)
}

func TestFormatProc(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~?~A" "~Ax" '(1 2) 3)`,
		Expect: `"1x3"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~@?~A" "~Ax" '(1 2) 3)`,
		Expect: `"(1 2)x3"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~?~A" t '(1 2) 3)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~?~A" "~A" t)`,
		Panics: true,
	}).Test(t)
}

func TestFormatA(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~A" 12)`,
		Expect: `"12"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~A" "abc")`,
		Expect: `"abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~@A" nil)`,
		Expect: `"nil"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:A" nil)`,
		Expect: `"()"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~5:A" "abc")`,
		Expect: `"abc  "`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v:A" 5 "abc")`,
		Expect: `"abc  "`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~#:A" "abc" 1 2 3 4)`,
		Expect: `"abc  "`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~1,2,3,'-:@A" 12)`,
		Expect: `"---12"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~A" (make-condition 'parse-error :message "raise"))`,
		Expect: `/"#<parse-error [0-9a-f]+>"/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~#A" (make-condition 'parse-error :message "raise"))`,
		Expect: `/"#<parse-error [0-9a-f]+>"/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~1,v:@A" t 12)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~1,,,v:@A" t 12)`,
		Panics: true,
	}).Test(t)
}

func TestFormatB(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~,,'_,4:B" 1234)`,
		Expect: `"100_1101_0010"`,
	}).Test(t)
}

func TestFormatC(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~C" #\A)`,
		Expect: `"A"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~C" #\space)`,
		Expect: `" "`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:C" #\space)`,
		Expect: `"Space"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:@C" #\space)`,
		Expect: `"Space"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~@C" #\space)`,
		Expect: `"#\Space"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~C" t)`,
		Panics: true,
	}).Test(t)
}

func TestFormatD(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~D" 1234)`,
		Expect: `"1234"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:D" 1234)`,
		Expect: `"1,234"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~8,,'_,2:@D" 1234)`,
		Expect: `"  +12_34"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:D" -1234)`,
		Expect: `"-1,234"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~D" (coerce 96 'octet))`,
		Expect: `"96"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~5:D" 'abc)`,
		Expect: `"  abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:D" 123456789012345678901234567890)`,
		Expect: `"123,456,789,012,345,678,901,234,567,890"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,,,-2D" -1234)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,,,0D" -1234)`,
		Panics: true,
	}).Test(t)
}

func TestFormatE(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~E" 12.345)`,
		Expect: `"1.2345e+1"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~E" -12.345)`,
		Expect: `"-1.2345e+1"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~E" -12345)`,
		Expect: `"-1.2345e+4"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~E" 123456789012345678901234567890)`,
		Expect: `"1.23456789012345678901234567890e+29"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~E" 'abc)`,
		Expect: `"abc"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "~,2E" 9.999)`,
		Expect: `"1.00e+0"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,2E" 1.2)`,
		Expect: `"1.20e+0"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~12,,,,,'_E" 12.345)`,
		Expect: `"___1.2345e+1"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~7,,,,'#E" 12.345)`,
		Expect: `"#######"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,4,2E" 12.345)`,
		Expect: `"1.2345e+01"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,3,2E" 12.345)`,
		Expect: `"1.235e+01"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,4,2,-1E" 12.345)`,
		Expect: `"0.0123e+03"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,,,0E" 12.345)`,
		Expect: `"0.12345e+2"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,,,6E" 12.345)`,
		Expect: `"123450.0e-4"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,4,2,-3@E" 12.345)`,
		Expect: `"+0.0001e+05"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "~v3E" -4 12.345)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,4,,7E" -4 12.345)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,4,,-4E" -4 12.345)`,
		Panics: true,
	}).Test(t)
}

func TestFormatF(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~F" 1234)`,
		Expect: `"1234.0"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,3F" 1234)`,
		Expect: `"1234.000"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,3,2@F" 1234)`,
		Expect: `"+123400.000"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "~F" 12.34)`,
		Expect: `"12.34"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,3F" 12.34)`,
		Expect: `"12.340"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,,-4F" 12.34)`,
		Expect: `"0.001234"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~,6,-2F" -12.34)`,
		Expect: `"-0.123400"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "~,2F" 12.345)`,
		Expect: `"12.35"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "~F" 'abc)`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestFormatG(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~G" 1234)`,
		Expect: `"1234.0"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~G" 1234e5)`,
		Expect: `"1.234e+8"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~G" 1234e-6)`,
		Expect: `"1.234e-3"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~10G" 1234e-8)`,
		Expect: `"0.00001234"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~10G" 1234e-9)`,
		Expect: `"  1.234e-6"`,
	}).Test(t)
}

func TestFormatI(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "x~Iy")`,
		Expect: `"xy"`,
	}).Test(t)
}

func TestFormatO(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~O" 1234)`,
		Expect: `"2322"`,
	}).Test(t)
}

func TestFormatP(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~D cat~P" 1 1)`,
		Expect: `"1 cat"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~D cat~:P" 1)`,
		Expect: `"1 cat"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~D cat~:P" 2)`,
		Expect: `"2 cats"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~D cr~:@P" 1)`,
		Expect: `"1 cry"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~D cr~:@P" 2)`,
		Expect: `"2 cries"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~D cr~@P" 2)`,
		Panics: true,
	}).Test(t)
}

func TestFormatR(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~@R" 1234)`,
		Expect: `"MCCXXXIV"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:@R" 1234)`,
		Expect: `"MCCXXXIIII"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:@R" 12340)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:@R" -123)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~R" 1234)`,
		Expect: `"one thousand two hundred thirty four"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:R" 1234)`,
		Expect: `"one thousand two hundred thirty fourth"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:R" -111)`,
		Expect: `"negative one hundred eleventh"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~R" 1000000000001)`,
		Expect: `"one trillion one"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~R" 0)`,
		Expect: `"zero"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:R" 0)`,
		Expect: `"zeroth"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~R" 300000000000000000000000000003)`,
		Expect: `"three hundred octillion three"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:R" t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:R")`,
		Panics: true,
	}).Test(t)
}

func TestFormatS(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~S" 12)`,
		Expect: `"12"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~S" "abc")`,
		Expect: `""abc""`,
	}).Test(t)
	// TBD
	// (&sliptest.Function{
	// 	Source: `(format nil "~S" (make-condition 'parse-error :message "raise"))`,
	// 	Expect: `/^"#<parse-error [0-9a-f]+>"$/`,
	// }).Test(t)
}

func TestFormatT(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "abc~,8@Tdef")`,
		Expect: `"abc     def"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "abc~3,4@Tdef")`,
		Expect: `"abc     def"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "abc~1,4@Tdef")`,
		Expect: `"abc def"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "abc~%~3,4@Tdef")`,
		Expect: `"abc
    def"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "abc~81,8@Tdef")`,
		Expect: `"abc                                                                                     def"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "abc~,8Tdef")`,
		Expect: `"abc     def"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "abc~2,4Tdef")`,
		Expect: `"abc     def"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "abc~%~1,4Tdef")`,
		Expect: `"abc
    def"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "abc~21,4Tdef")`,
		Expect: `"abc                                                                                 def"`,
	}).Test(t)
}

func TestFormatW(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(setq value '(1 2 3 4 5 6 7 8 9 (8 (7 (6 (5 (4 (3 (2 (1))))))))))`, scope).Eval(scope, nil)
	origMargin := scope.Get(slip.Symbol("*print-right-margin*"))
	origPretty := scope.Get(slip.Symbol("*print-pretty*"))
	origLevel := scope.Get(slip.Symbol("*print-level*"))
	defer func() {
		scope.Set(slip.Symbol("*print-right-margin*"), origMargin)
		scope.Set(slip.Symbol("*print-pretty*"), origPretty)
		scope.Set(slip.Symbol("*print-level*"), origLevel)
	}()
	scope.Set(slip.Symbol("*print-right-margin*"), slip.Fixnum(40))
	scope.Set(slip.Symbol("*print-pretty*"), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(format nil "~W" value)`,
		Expect: `"(1 2 3 4 5 6 7 8 9 (8 (7 (6 (5 (4 (3 (2 (1)))))))))"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(format nil "~:W" value)`,
		Expect: `"(1 2 3 4 5 6 7 8 9
   (8 (7 (6 (5 (4 (3 (2 (1)))))))))"`,
	}).Test(t)
	scope.Set(slip.Symbol("*print-level*"), slip.Fixnum(3))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(format nil "~W" value)`,
		Expect: `"(1 2 3 4 5 6 7 8 9 (8 (7 #)))"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(format nil "~@W" value)`,
		Expect: `"(1 2 3 4 5 6 7 8 9 (8 (7 (6 (5 (4 (3 (2 (1)))))))))"`,
	}).Test(t)
}

func TestFormatX(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~,,'_,2:X" 1234)`,
		Expect: `"4_d2"`,
	}).Test(t)
}

func TestFormatTilde(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~~")`,
		Expect: `"~"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~3~")`,
		Expect: `"~~~"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v~" 3)`,
		Expect: `"~~~"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v~" t)`,
		Panics: true,
	}).Test(t)
}

func TestFormatCond(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~[ann~]" 0)`,
		Expect: `"ann"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~1[ann~;bob~;candy~]")`,
		Expect: `"bob"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v[ann~;bob~;candy~]" 1)`,
		Expect: `"bob"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~[ann~;bob~:;candy~]" 4)`,
		Expect: `"candy"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~[ann~;b~[xxx~]b~;candy~]" 1 0)`,
		Expect: `"bxxxb"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v[ann~;bob~;candy~]" t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~[ann~;bob~;candy~]" t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:@[ann~;bob~:;candy~]" 0)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~[ann~;bob~;candy~@]" 0)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~[ann~;bob~;candy" 0)`,
		Panics: true,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "~:[alt~;con~]" t)`,
		Expect: `"con"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:[alt~;con~]" nil)`,
		Expect: `"alt"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~:[alt~;con~;bad~]" nil)`,
		Panics: true,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "~@[con~] ~A" 5)`,
		Expect: `"con 5"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~@[con~] ~A" 5)`,
		Expect: `"con 5"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~@[con~] ~A" nil 3)`,
		Expect: `" 3"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~@[con~:;extra~] ~A" nil)`,
		Panics: true,
	}).Test(t)
}

func TestFormatIter(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "names:~{ ~A~}" '(ann bob candy))`,
		Expect: `"names: ann bob candy"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "names:~1{ ~A~}" '(ann bob candy))`,
		Expect: `"names: ann"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "names:~0{ ~A~}" '(ann bob candy))`,
		Expect: `"names:"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "names:~{ ~A~}" t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "names:~{ ~A~^~}" '(ann bob candy))`,
		Expect: `"names: ann"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "names:~@{ ~A~}" 'ann 'bob 'candy)`,
		Expect: `"names: ann bob candy"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "names:~:{ ~A~}" '((ann) (bob) (candy)))`,
		Expect: `"names: ann bob candy"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "names:~1:{ ~A~}" '((ann) (bob) (candy)))`,
		Expect: `"names: ann"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "names:~:{ ~A~:}" '())`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "names:~:{ ~A~}" '(t))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "names:~:{ ~A~}" t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "names:~{ ~A~:}" '())`,
		Panics: true,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "names:~:@{ ~A~}" '(ann) '(bob) '(candy))`,
		Expect: `"names: ann bob candy"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "names:~:@{ ~A~}" '(ann) nil '(candy))`,
		Panics: true,
	}).Test(t)
}

func TestFormatPage(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~|")`,
		Expect: "\"\f\"",
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~3|")`,
		Expect: "\"\f\f\f\"",
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v|" 3)`,
		Expect: "\"\f\f\f\"",
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~v|" t)`,
		Panics: true,
	}).Test(t)
}
