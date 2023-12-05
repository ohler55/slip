// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"time"

	"github.com/ohler55/ojg"
	"github.com/ohler55/ojg/alt"
	"github.com/ohler55/slip"
)

var (
	// Pkg is the Bag package.
	Pkg = slip.Package{
		Name:      "bag",
		Nicknames: []string{},
		Doc:       "Home of symbols defined for the bag package.",
		PreSet:    slip.DefaultPreSet,
	}

	options = ojg.DefaultOptions
)

func init() {
	Pkg.Initialize(map[string]*slip.VarVal{
		"*bag-time-format*": {
			Get: getTimeFormat,
			Set: setTimeFormat,
			Doc: "is the format for writing time as a string in bag-format and the format for parsing time.",
		},
		"*bag-time-wrap*": {
			Get: getTimeWrap,
			Set: setTimeWrap,
			Doc: `if non-nil then the writing and parsing of time is as a hash-map with a key of
the _*bag-time_wrap*_ value and the time encoded according to the _*bag-time-format*_.`,
		},
	})
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*bag*", &Pkg)
	slip.DefConstant(slip.Symbol("*rfc3339nano*"), slip.String(time.RFC3339Nano), "")
}

func getTimeFormat() slip.Object {
	if 0 < len(options.TimeFormat) {
		return slip.String(options.TimeFormat)
	}
	return nil
}

func setTimeFormat(value slip.Object) {
	switch tv := value.(type) {
	case nil:
		options.TimeFormat = ""
	case slip.Symbol:
		options.TimeFormat = string(tv)
	case slip.String:
		options.TimeFormat = string(tv)
	default:
		slip.PanicType("*bag-time-format*", value, "string")
	}
	updateConverter()
}

func getTimeWrap() slip.Object {
	if 0 < len(options.TimeWrap) {
		return slip.String(options.TimeWrap)
	}
	return nil
}

func setTimeWrap(value slip.Object) {
	switch tv := value.(type) {
	case nil:
		options.TimeWrap = ""
	case slip.Symbol:
		options.TimeWrap = string(tv)
	case slip.String:
		options.TimeWrap = string(tv)
	default:
		slip.PanicType("*bag-time-wrap*", value, "string")
	}
	updateConverter()
}

func updateConverter() {
	if len(options.TimeFormat) == 0 {
		options.Converter = nil
		return
	}
	if 0 < len(options.TimeWrap) {
		options.Converter = &alt.Converter{
			Map: []func(val map[string]interface{}) (interface{}, bool){
				func(val map[string]interface{}) (interface{}, bool) {
					if len(val) == 1 {
						switch tv := val[options.TimeWrap].(type) {
						case string:
							for _, layout := range []string{
								time.RFC3339Nano,
								time.RFC3339,
								"2006-01-02",
								options.TimeFormat,
							} {
								if t, err := time.ParseInLocation(layout, tv, time.UTC); err == nil {
									return t, true
								}
							}
						case int64:
							return time.Unix(0, tv), true
						}
					}
					return val, false
				},
			},
		}
	} else {
		switch options.TimeFormat {
		case "nano":
			options.Converter = &ojg.TimeNanoConverter
		case time.RFC3339Nano, "rfc3339":
			options.Converter = &ojg.TimeRFC3339Converter
		case "second":
			options.Converter = &ojg.Converter{
				Float: []func(val float64) (interface{}, bool){
					func(val float64) (interface{}, bool) {
						if 946684800.0 <= val && val <= 2524608000.0 { // 2000-01-01 <= val <= 2050-01-01
							sec := int64(val)
							nano := int64((val - float64(sec)) * 1_000_000_000.0)
							return time.Unix(sec, nano), true
						}
						return val, false
					},
				},
			}
		default:
			options.Converter = &ojg.Converter{
				String: []func(val string) (interface{}, bool){
					func(val string) (interface{}, bool) {
						if 6 <= len(val) { // minimal len for year month and day.
							if t, err := time.ParseInLocation(options.TimeFormat, val, time.UTC); err == nil {
								return t, true
							}
						}
						return val, false
					},
				},
			}
		}
	}
}
