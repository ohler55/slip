// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Format{Function: slip.Function{Name: "format", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "format",
			Args: []*slip.DocArg{
				{
					Name: "destination",
					Type: "output-stream|string|t|nil",
					Text: `The destination to write to. If _t_ then write to _*standard-output*.
If _nil_ then return a string.`,
				},
				{
					Name: "control",
					Type: "string",
					Text: `The control string for the formatting.`,
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to be used as arguments to the _control_ string.",
				},
			},
			Return: "string|nil",
			Text: `__format__ output as directed by the _control_ string and write to the _destination_.
If _destination_ is _nil_ then a string is returned otherwise _nil_ is returned.


A control directive begins a _~_ character followed by control characters. A directive has the form of:

   ~[_parameter_,]...[_modifier_]<directive>


Common LISP defines a language for handling a variety of control directive operations. A subset of
those operations are implemented. An addition operator for inline evaluation of code is included as well.


Control directives and operations are:

 ___Directive___   ___Documentation___
 _          _  _                                                               _
 __~<newline>__  The _newline_ character and any following whitespace are ignored.
             The : modifier indicates only the _newline_ is ignored. The @
             modifier indicates the _newline_ is not ignored but any trailing
             whitespace is ignored.

 __~$__          Outputs a number in a monetary format. The general form is:
               ~d,n,w,padchar__$__
             Parameters are:
             __d__       - number of digits after the decimal point
             __n__       - minimum number of digits before the decimal point
             __w__       - minimum total width
             __padchar__ - pad character
             Padding and sign are output before digits. If a _:_ modified is
             included the sign is output before padding otherwise the sign
             is output after the padding.

 __~%__          Outputs a _newline_ character. A positive integer parameter
             indicates the number of _newline_ characters to output. The
             general form is ~_n_%.

 __~&__          Outputs a _newline_ if the previous output character was not a
             _newline_. A positive integer parameter indicates the number of
             _newline_ characters that should be output. The general form
             is ~_n_%.

 __~(__          Performs a case conversion on the string delimited by the
             starting _(_ and a closing _)_. The general form is ~(string~)
             Conversion is driven by the modifiers
             __none__ - all words are converted to lowercase
             __:__    - all words are converted to lowercase and then capitalized
             __@__    - all words are converted to lowercase and then the first
                    word is capitalized
             __:@__   - all words are converted to uppercase

 __~*__          Moves the argument position pointer according to the modifier
             and _n_ prefix parameter. The default value of _n_ is 1. The
             general form is ~_n_*. Modifiers are:
             __none__ - move past or skip the next _n_ arguments
             __:__    - move back _n_ arguments
             __@__    - move to the _n_th zero based position

 __~/__          Calls a function identified by the _name_ with the general form
             of: ~__/__name__/__
             The function bound to the _name_ must take 4 arguments. The first
             argument is an _output-stream_. The second is the format argument
             corresponding to the directive. The third is true if the _:_
             modifier was specified while the fourth is true if the _@_
             modifier was specified.

 __~;__          Separator for the __~[__ and __~<__ directives.

 __~<__          Justify the contents of the string between the start __~<__
             directive and the __~>__ directive. The general form is:
               ~mincol,colinc,minpad,padchar__<__string__~>__
             Segments are separated by the __~;__ directive. The __~^__ directive
             can be used to terminate the justification. The string can
             include formatting directives.
             Modifiers are:
             __none__ - first segment is left justified and last is right
                    justified. If only one segment it is right justified.
             __:__    - add space before the first segment
             __@__    - add space after the last segment

 __~=__          Evaluate the expression between the _~=_ delimiters and output
             using the Aesthetic (__~A__) directive. Example: ~=(1+ x)~=. This
             is a non-standard directive.

 __~>__          Terminates a justification directive.

 __~?__          Referred to as the Recursive Processing directive it expects
             the next argument to be a control string and the argument after
             the control to a list of arguments to be consumed by processing
             the control string. With the _@_ modifier the control string is
             consumed and the arguments for that control string are the
             remaining arguments to the original format call. Example:
               (format nil "~?~A" "~Ax" '(1 2) 3) => "1x3"

 __~A__          Output the aesthetic representation of an argument in human
             friendly format. The full form is:
               ~mincol,colinc,minpad,padchar__A__
             Modifiers are:
             __:__    - _nil_ is output as () instead of nil
             __@__    - left pad the output
             Parameters are:
             __mincol__  - minimum width
             __colinc__  - incremental padding when needed (like tab stops)
             __minpad__  - minimum padding
             __padchar__ - padding character (e.g., '.)

 __~B__          Output an integer as binary (radix 2). The full form is:
               ~mincol,padchar,commachar,comma-interval__B__
             Modifiers and parameters are the same as the Decimal
             directive (__~D__).

 __~C__          Character arguments are output with the Character directive.
             Modifiers are:
             __none__ - output is the character itself
             __:__    - special characters are spelled out. For example a
                    space (" ") is output as "Space"
             __@__    - output is suitable for reading by a LISP reader

 __~D__          Output an integer according to the modifiers and parameters. If
             a non-integer argument is given then the Aesthetic directive is
             used. The full form is:
               ~mincol,padchar,commachar,comma-interval__D__
             Modifiers are:
             __:__ - commas are inserted every _comma-interval_
             __@__ - force the sign (+ or -) to be prepended
             Parameters are:
             __mincol__         - minimum width
             __padchar__        - padding character (e.g., #\Tab)
             __commachar__      - comma character (e.g., #\space)
             __comma-interval__ - interval between commas

 __~E__          Output a number as a floating point. The full form is:
               ~w,d,e,k,overflowchar,padchar,exponentchar__E__
             If the _@_ modifier is present the sign is always output.
             Parameters are:
             __w__            - minimum width.
             __d__            - number of places after the decimal
             __e__            - number of places for the exponent digits
             __k__            - scaling factor to apply in as in num*10^scale
             __overflowchar__ - character to use in place of output that does
                            not fit in the _width_. If not provided _width_ is
                            ignored
             __padchar__      - character to use for padding output less than
                            _width_
             __exponentchar__ - character to use for the exponent separator in
                            place of 'e'

 __~F__          Output a number as a fixed format floating point decimal
             notation without an exponent. The full form is:
               ~w,d,k,overflowchar,padchar__F__
             Modifiers and parameters and the same as the Exponential (__~E__)
             directive.

 __~G__          Output a number  in general floating point notation which uses
             either the __~F__ or __~E__ directive based on the magnitude of the
             exponent. The full form is:
               ~w,d,e,k,overflowchar,padchar,exponentchar__G__

 __~I__          Ignored

 __~O__          Output an integer as octal (radix 8). The full form is:
               ~mincol,padchar,commachar,comma-interval__O__
             Modifiers and parameters are the same as the Decimal
             directive (__~D__).

 __~P__          A poor attempt at pluralization. If the argument is 1 then
             nothing is output otherwise the lowercase character 's' is output.
             Modifiers are:
             __:__  - backup one argument before evaluation
             __@__  - output 'y' if argument is 1 otherwise 'ies'
             __:@__ - output 'y' if the previous argument is 1 otherwise 'ies'

 __~R__          Output an integer in a specific radix. The full form is:
               ~radix,mincol,padchar,commachar,comma-interval__R__
             Modifiers and parameters are the same as the Decimal
             directive (__~D__) but with the addition of the _radix_ parameter.

 __~S__          Output the standard notation for the argument. The Standard
             notation is the same as the Aesthetic (__~A__) directive except
             that escape and quote characters are output so that output is
             suitable for reading by a LISP reader.

 __~T__          Output is aligned in columns using space characters as padding
             with the Tabulate directive. The full form is:
               ~colnum,colinc__T__
             The default value for _colnum_ and _colinc_ is 1.
             Parameters are:
             __colnum__ - column number
             __colinc__ - column width
             Outputs enough spaces to reach _colnum_ or the first column after
             _colnum_ assuming _colinc_ is the width of each column.

 __~W__          Output an object using the current printer control variables.
             Modifiers are:
             __:__  - bind _*print-pretty*_ to true
             __@__  - bind _*print-level*_ and _*print-length*_ to nil
             __:@__ - combines __:__ and __@__

 __~X__          Output an integer as hexadecimal (radix 16). The full form is:
               ~mincol,padchar,commachar,comma-interval__X__
             Modifiers and parameters are the same as the Decimal
             directive (__~D__).

 __~[__          Conditional output is provided with this directive. The general
             forms are:
               ~__[__str0__~;__str1__~;__...__~;__strn__~]__
               ~__[__str0__~;__str1__~;__...__~;__strn__~:;__default__~]__
               ~__:[__alternative__~;__consequent__~]__
               ~__@[__consequent__~]__
             The first form expects an integer argument that identifies the
             position of the string to be output. If the argument is below
             zero or greater than the number of strings then nothing is
             output. The second form is the same as the first except that if
             the argument is out of range the _default_ is output. The third
             form selects the _alternative_ if the argument is _nil_ otherwise
             the _consequent_ is selected. The fourth form selects the
             _consequent_ checks the argument. If _true_ then the argument is
             not consumed and _consequent_ is selected. If the argument is _nil_
             then it is consumed and no output occurs.

 __~]__          Terminate a __~[__ directive.

 __~^__          Terminate a __~{__ or __~<__ directive or if no arguments remain.

 __~{__          An Iteration directive iterates over arguments and outputs
             those arguments according to the format string that delimited
             by the _~{_ and _~}_ sequences. The general form is ~_n_{str~}. The _n_
             parameters is the maximum number of iterations. Modifiers are:
             __none__ - the iterator argument must be a list that is consumed in
                    progressively for each iteration
             __:__    - the iterator argument must be a list of lists with the
                    each list element being consumed by one iteration
             __@__    - the iteration arguments are consumed from the format
                    arguments
             __:@__   - the iteration consumes format arguments that must be
                    lists

 __~}__          Terminates an iteration. If a _:_ modifier is present then the
             iteration will be processed at least once.

 __~|__          Outputs a page separator (\f or 0x0C). A positive _n_ parameter
             specifies the number page separators to output. The general
             form is ~_n_|.

 __~~__          Outputs a tilde character. An integer prefix parameter is
             supported. A positive _n_ parameter specifies the number of tilde
             characters to output. The general form is ~_n_~.


In addition, a parameters that apply to all directives that have numeric
parameters, _v_ can be used to consume the next argument and use it as a
parameter. As an example (format nil "~v$" 3 1.2345) => "1.234". Additionally,
the _#_ parameter will be replaced with the number of remaining arguments.

`,
			Examples: []string{
				`(format nil "number: ~A" 123) => "number: 123"`,
			},
		}, &slip.CLPkg)
}

// Format represents the format function.
type Format struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Format) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 2 {
		slip.PanicArgCount(f, 2, -1)
	}
	var w io.Writer
	switch ta := args[0].(type) {
	case nil:
		// leave w as nil
	case io.Writer:
		w = ta
	default:
		if ta == slip.True {
			if w, _ = s.Get("*standard-output*").(io.Writer); w != nil {
				break
			}
		}
		slip.PanicType("destination", ta, "output-stream")
	}
	out := FormatArgs(s, args[1:])
	if w == nil {
		return slip.String(out)
	}
	if _, err := w.Write(out); err != nil {
		panic(err)
	}
	return nil
}

// FormatArgs uses the provided args as if a format function would to append
// to a buffer.
func FormatArgs(s *slip.Scope, args slip.List) []byte {
	cs, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("control", args[0], "string")
	}
	ctrl := control{scope: s, str: []byte(cs), args: args[1:], argPos: 0}
	ctrl.end = len(ctrl.str)
	ctrl.process()

	return ctrl.out
}
