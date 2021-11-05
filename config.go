/*
 * Copyright (C) 2019-2020 Red Dove Consultants Ltd. All rights reserved.
 */
package config

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"math/cmplx"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
)

type Mapping map[string]Any

type MapWrapper struct {
	data   *Mapping
	config *Config
}

func newMapWrapper(config *Config, data *Mapping) MapWrapper {
	return MapWrapper{data, config}
}

func (self MapWrapper) String() string {
	parts := make([]string, 0)
	for k, _ := range *self.data {
		var s string

		v, err := self.Get(k)
		if err != nil {
			s = "???"
		} else {
			s = fmt.Sprintf("%s: %s", k, v)
		}
		parts = append(parts, s)
	}
	return fmt.Sprintf("{%s}", strings.Join(parts, ", "))
}

var identifierPattern = regexp.MustCompile(`^([\p{L}_]([\p{L}\p{N}_])*)$`)

func isIdentifier(s string) bool {
	parts := identifierPattern.FindStringSubmatch(s)
	return parts != nil
}

func (self *MapWrapper) Get(key string) (Any, error) {
	var result Any
	var err error
	var ok bool

	if result, ok = (*self.data)[key]; ok {
		result, err = self.config.evaluated(result)
	} else if !isIdentifier(key) {
		result, err = self.config.getFromPath(key)
	} else {
		return nil, errFmt(nil, "Not found in configuration: %s", key)
	}
	return result, err
}

func (self *MapWrapper) baseGet(key string) (Any, error) {
	if result, ok := (*self.data)[key]; ok {
		return result, nil
	} else {
		return nil, errFmt(nil, "Not found in configuration: %s", key)
	}
}

func (self *MapWrapper) AsDict() (Mapping, error) {
	var err error
	var key string
	var item Any

	result := make(Mapping)

	for key, item = range *self.data {
		item, err = self.config.evaluated(item)
		if err != nil {
			break
		}
		switch v := item.(type) {
		case SeqWrapper:
			item, err = v.AsList()
		case MapWrapper:
			item, err = v.AsDict()
		case *Config:
			item, err = v.AsDict()
		}
		if err != nil {
			break
		}
		result[key] = item
	}
	return result, err
}

type SeqWrapper struct {
	data   *Sequence
	config *Config
}

func newSeqWrapper(config *Config, data *Sequence) SeqWrapper {
	return SeqWrapper{data, config}
}

func (self SeqWrapper) String() string {
	parts := make([]string, 0)
	for i, _ := range *self.data {
		var s string

		v, err := self.Get(i)
		if err != nil {
			s = "???"
		} else {
			s = fmt.Sprintf("%s", v)
		}
		parts = append(parts, s)
	}
	return fmt.Sprintf("[%s]", strings.Join(parts, ", "))
}

func (self *SeqWrapper) baseGet(index int) Any {
	return (*self.data)[index]
}

func (self *SeqWrapper) Get(index int) (Any, error) {
	v := self.baseGet(index)
	return self.config.evaluated(v)
}

func (self *SeqWrapper) AsList() (Sequence, error) {
	var err error
	var item Any

	result := make(Sequence, 0)

	for _, item = range *self.data {
		item, err = self.config.evaluated(item)
		if err != nil {
			break
		}
		switch v := item.(type) {
		case SeqWrapper:
			item, err = v.AsList()
		case MapWrapper:
			item, err = v.AsDict()
		case *Config:
			item, err = v.AsDict()
		}
		if err != nil {
			break
		}
		result = append(result, item)
	}
	return result, err
}

type StringConverter func(string, *Config) Any

type Config struct {
	NoDuplicates      bool
	StrictConversions bool
	IncludePath       []string
	RootDir           string
	Path              string
	parent            *Config
	data              *MapWrapper
	cache             *Mapping
	Context           *Mapping
	evaluator         *evaluator
	converter         StringConverter
}

func (self *Config) String() string {
	n := 0
	suffix := "s"
	if self.data != nil {
		n = len(*(*self.data).data)
		if n == 1 {
			suffix = ""
		}
	}
	return fmt.Sprintf("Config(\"%s\" [%d item%s])", filepath.Base(self.Path), n, suffix)
}

var isoDatetimePattern = regexp.MustCompile(`^(\d{4})-(\d{2})-(\d{2})(([ T])(((\d{2}):(\d{2}):(\d{2}))(\.\d{1,6})?(([+-])(\d{2}):(\d{2})(:(\d{2})(\.\d{1,6})?)?)?))?$`)
var envValuePattern = regexp.MustCompile(`^\$(\w+)(\|(.*))?$`)
var interpolationPattern = regexp.MustCompile(`\$\{([^}]+)\}`)

// var colonObjectPattern = regexp.MustCompile(`^([A-Za-z_]\w*([/.][A-Za-z_]\w*)*)(:([A-Za-z_]\w*))?$`)

func defaultStringConverter(s string, cfg *Config) Any {
	var result Any = s
	var parts []string

	parts = isoDatetimePattern.FindStringSubmatch(s)
	if parts != nil {
		year, _ := strconv.Atoi(parts[1])
		month, _ := strconv.Atoi(parts[2])
		day, _ := strconv.Atoi(parts[3])
		var hour = 0
		var minute = 0
		var second = 0
		var nanosecond = 0
		var offsetHour = 0
		var offsetMinute = 0
		var offsetSecond = 0
		hasTime := parts[5] != ""
		loc, _ := time.LoadLocation("UTC")
		if hasTime {
			hour, _ = strconv.Atoi(parts[8])
			minute, _ = strconv.Atoi(parts[9])
			second, _ = strconv.Atoi(parts[10])
			if parts[11] != "" {
				fv, _ := strconv.ParseFloat(parts[11], 64)
				nanosecond = int(fv * 1.0e9)
			}
			hasOffset := parts[13] != ""
			if hasOffset {
				var sign int

				if parts[13] == "-" {
					sign = -1
				} else {
					sign = 1
				}
				offsetHour, _ = strconv.Atoi(parts[14])
				offsetMinute, _ = strconv.Atoi(parts[15])
				if parts[17] == "" {
					offsetSecond = 0
				} else {
					offsetSecond, _ = strconv.Atoi(parts[17])
				}
				loc = time.FixedZone("", sign*(offsetHour*3600+offsetMinute*60+offsetSecond))
			}
		}
		result = time.Date(year, time.Month(month), day, hour, minute, second, nanosecond, loc)
	} else {
		parts = envValuePattern.FindStringSubmatch(s)
		if parts != nil {
			name := parts[1]
			hasPipe := parts[2] != ""
			if value, ok := os.LookupEnv(name); ok {
				result = value
			} else {
				if hasPipe {
					result = parts[3]
				}
			}
		} else {
			matches := interpolationPattern.FindAllStringSubmatchIndex(s, -1)
			if matches != nil {
				cp := 0
				failed := false
				sparts := make([]string, 0)
				for _, match := range matches {
					sp := match[0]
					ep := match[1]
					path := s[match[2]:match[3]]
					if cp < sp {
						sparts = append(sparts, s[cp:sp])
					}
					v, err := cfg.Get(path)
					if err != nil {
						failed = true
						break
					}
					sparts = append(sparts, fmt.Sprintf("%v", v))
					cp = ep
				}
				if !failed {
					if cp < len(s) {
						sparts = append(sparts, s[cp:])
					}
					result = strings.Join(sparts, "")
				}
			}
		}
	}
	return result
}

type evaluator struct {
	config   *Config
	refsSeen *map[UnaryNode]bool
}

func newEvaluator(config *Config) evaluator {
	rs := make(map[UnaryNode]bool)
	return evaluator{config, &rs}
}

var scalarTokens = map[tokenKind]bool{
	Integer: true,
	Float:   true,
	Complex: true,
	True:    true,
	False:   true,
	None:    true,
	String:  true,
}

func sameFile(s1 string, s2 string) bool {
	var info1 os.FileInfo
	var info2 os.FileInfo
	var err error

	if info1, err = os.Stat(s1); err != nil {
		return false
	}
	if info2, err = os.Stat(s2); err != nil {
		return false
	}
	return os.SameFile(info1, info2)
}

func (self *evaluator) evalAt(node UnaryNode) (Any, error) {
	var result Any
	var operand Any
	var err error

	if operand, err = self.evaluate(node.operand); err == nil {
		if p, ok := operand.(string); !ok {
			err = errFmt(nil, "@ operand must be a string, but is: %#v", operand)
		} else {
			var fn string

			found := false
			if filepath.IsAbs(p) {
				if _, err = os.Stat(p); err == nil {
					fn = p
					found = true
				}
			} else {
				fn = filepath.Join(self.config.RootDir, p)
				if _, err = os.Stat(fn); err == nil {
					found = true
				} else {
					for _, dn := range self.config.IncludePath {
						fn = filepath.Join(dn, p)
						if _, err = os.Stat(fn); err == nil {
							found = true
							break
						}
					}
				}
			}
			if !found {
				err = errFmt(nil, "Unable to locate %s", p)
			} else {
				if self.config.Path != "" && sameFile(self.config.Path, fn) {
					err = errFmt(nil, "Configuration cannot include itself: %s", filepath.Base(fn))
				} else {
					var f *os.File

					f, err = os.Open(fn)
					if err == nil {
						var reader io.Reader
						var parser Parser

						defer closeFile(f)
						reader = bufio.NewReader(f)
						parser, err = NewParser(&reader)
						if err == nil {
							var node Any

							node, err = parser.Container()
							if err == nil {
								switch v := node.(type) {
								case []keyValue:
									var mapping MapWrapper

									cfg := NewConfig()
									cfg.NoDuplicates = self.config.NoDuplicates
									cfg.StrictConversions = self.config.StrictConversions
									mapping, err = cfg.wrapMapping(v)
									if err == nil {
										err = cfg.setPath(fn)
										if err == nil {
											cfg.data = &mapping
											cfg.parent = self.config
											cfg.Context = self.config.Context
											cfg.IncludePath = self.config.IncludePath[:]
											if self.config.cache != nil {
												cache := make(Mapping)
												cfg.cache = &cache
											}
											result = cfg
										}
									}
								case Sequence:
									result = newSeqWrapper(self.config, &v)
								default:
									result = v
								}
							}
						}
					}
				}
			}
		}
	}
	return result, err
}

func isInteger(v Any) bool {
	switch v.(type) {
	case int64:
		return true
	default:
		return false
	}
}

func isFloat(v Any) bool {
	switch v.(type) {
	case float64:
		return true
	default:
		return false
	}
}

func isString(v Any) bool {
	switch v.(type) {
	case string:
		return true
	default:
		return false
	}
}

func isComplex(v Any) bool {
	switch v.(type) {
	case complex128:
		return true
	default:
		return false
	}
}

func isMapping(v Any) bool {
	switch v.(type) {
	case MapWrapper:
		return true
	default:
		return false
	}
}

func isSequence(v Any) bool {
	switch v.(type) {
	case SeqWrapper:
		return true
	default:
		return false
	}
}

func toFloat(v Any) float64 {
	switch v.(type) {
	case int64:
		return float64(v.(int64))
	case float64:
		return v.(float64)
	default:
		panic(fmt.Sprintf("unable to convert %v to float64", v))
	}
}

func toComplex(v Any) complex128 {
	switch v.(type) {
	case complex128:
		return v.(complex128)
	case int64:
		return complex(float64(v.(int64)), 0.0)
	case float64:
		return complex(v.(float64), 0.0)
	default:
		panic(fmt.Sprintf("unable to convert %v to float64", v))
	}
}

func mergeDicts(target *Mapping, source *Mapping) {
	for k, v := range *source {
		if tv, ok := (*target)[k]; ok {
			if tvm, ok := tv.(Mapping); ok {
				if vm, ok := v.(Mapping); ok {
					mergeDicts(&tvm, &vm)
					continue
				}
			}
		}
		(*target)[k] = v
	}
}

func mergeMappings(map1 MapWrapper, map2 MapWrapper) (MapWrapper, error) {
	var result MapWrapper
	var err error
	var mm Mapping

	mm, err = map1.AsDict()
	if err == nil {
		var m2 Mapping

		m2, err = map2.AsDict()
		if err == nil {
			mergeDicts(&mm, &m2)
			result = newMapWrapper(map1.config, &mm)
		}
	}
	return result, err
}

func (self *evaluator) addValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isFloat(lhs) || isFloat(rhs) {
		result = toFloat(lhs) + toFloat(rhs)
	} else if isComplex(lhs) || isComplex(rhs) {
		result = toComplex(lhs) + toComplex(rhs)
	} else if isInteger(lhs) && isInteger(rhs) {
		result = lhs.(int64) + rhs.(int64)
	} else if isString(lhs) && isString(rhs) {
		result = fmt.Sprintf("%v%v", lhs, rhs)
	} else if isSequence(lhs) && isSequence(rhs) {
		s := lhs.(SeqWrapper)
		data, err := s.AsList()
		if err == nil {
			var lv Sequence

			s := rhs.(SeqWrapper)
			lv, err = s.AsList()
			if err == nil {
				data = append(data, lv...)
				result = newSeqWrapper(s.config, &data)
			}
		}
	} else if isMapping(lhs) && isMapping(rhs) {
		result, err = mergeMappings(lhs.(MapWrapper), rhs.(MapWrapper))
	} else {
		err = errFmt(nil, "unable to add %s and %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) subtractValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isFloat(lhs) || isFloat(rhs) {
		result = toFloat(lhs) - toFloat(rhs)
	} else if isInteger(lhs) && isInteger(rhs) {
		result = lhs.(int64) - rhs.(int64)
	} else if isComplex(lhs) || isComplex(rhs) {
		result = toComplex(lhs) - toComplex(rhs)
	} else if isMapping(lhs) && isMapping(rhs) {
		var md = make(Mapping)
		m := newMapWrapper(lhs.(MapWrapper).config, &md)
		for k, v := range *lhs.(MapWrapper).data {
			if _, ok := (*rhs.(MapWrapper).data)[k]; !ok {
				md[k] = v
			}
		}
		result = m
	} else {
		err = errFmt(nil, "unable to subtract %s from %s", rhs, lhs)
	}
	return result, err
}

func (self *evaluator) multiplyValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isFloat(lhs) || isFloat(rhs) {
		result = toFloat(lhs) * toFloat(rhs)
	} else if isInteger(lhs) && isInteger(rhs) {
		result = lhs.(int64) * rhs.(int64)
	} else {
		err = errFmt(nil, "unable to multiply %s and %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) divideValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isFloat(lhs) || isFloat(rhs) {
		result = toFloat(lhs) / toFloat(rhs)
	} else if isInteger(lhs) && isInteger(rhs) {
		result = float64(lhs.(int64)) / float64(rhs.(int64))
	} else {
		err = errFmt(nil, "unable to divide %s by %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) integerDivideValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isFloat(lhs) || isFloat(rhs) {
		result = int64(toFloat(lhs) / toFloat(rhs))
	} else if isInteger(lhs) && isInteger(rhs) {
		result = lhs.(int64) / rhs.(int64)
	} else {
		err = errFmt(nil, "unable to integer divide %s by %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) moduloValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isInteger(lhs) && isInteger(rhs) {
		result = lhs.(int64) % rhs.(int64)
	} else {
		err = errFmt(nil, "unable to compute %s modulo %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) leftShiftValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isInteger(lhs) && isInteger(rhs) {
		result = lhs.(int64) << rhs.(int64)
	} else {
		err = errFmt(nil, "unable to left-shift %s and %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) rightShiftValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isInteger(lhs) && isInteger(rhs) {
		result = lhs.(int64) >> rhs.(int64)
	} else {
		err = errFmt(nil, "unable to right-shift %s and %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) bitOrValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isInteger(lhs) && isInteger(rhs) {
		result = lhs.(int64) | rhs.(int64)
	} else if isMapping(lhs) && isMapping(rhs) {
		result, err = mergeMappings(lhs.(MapWrapper), rhs.(MapWrapper))
	} else {
		err = errFmt(nil, "unable to bitwise-or %s and %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) bitAndValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isInteger(lhs) && isInteger(rhs) {
		result = lhs.(int64) & rhs.(int64)
	} else {
		err = errFmt(nil, "unable to bitwise-and %s and %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) bitXorValues(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isInteger(lhs) && isInteger(rhs) {
		result = lhs.(int64) ^ rhs.(int64)
	} else {
		err = errFmt(nil, "unable to bitwise-xor %s and %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) exponentiate(lhs Any, rhs Any) (Any, error) {
	var result Any
	var err error

	if isFloat(lhs) || isFloat(rhs) {
		result = math.Pow(toFloat(lhs), toFloat(rhs))
	} else if isComplex(lhs) || isComplex(rhs) {
		result = cmplx.Pow(toComplex(lhs), toComplex(rhs))
	} else if isInteger(lhs) && isInteger(rhs) {
		result = int64(math.Pow(toFloat(lhs), toFloat(rhs)))
	} else {
		err = errFmt(nil, "unable to exponentiate %s with %s", lhs, rhs)
	}
	return result, err
}

func (self *evaluator) negate(v Any) (Any, error) {
	var result Any
	var err error

	if isInteger(v) {
		result = -v.(int64)
	} else if isFloat(v) {
		result = -v.(float64)
	} else if isComplex(v) {
		result = -v.(complex128)
	} else {
		err = errFmt(nil, "unable to negate %s", v)
	}
	return result, err
}

func (self *evaluator) evaluate(value Any) (Any, error) {
	var result Any
	var err error

	if value != nil {
		switch v := value.(type) {
		case bool:
			result = v
		case int64:
			result = v
		case string:
			result = v
		case Token:
			if _, ok := scalarTokens[v.kind]; ok {
				result = v.value
			} else if v.kind == Word {
				if self.config.Context == nil {
					err = errFmt(&v.start, "No context to look up variables'")
				} else if lv, ok := (*self.config.Context)[v.text]; ok {
					result = lv
				} else {
					err = errFmt(&v.start, "Unknown variable '%s'", v.text)
				}
			} else if v.kind == BackTick {
				if sv, ok := v.value.(string); ok {
					result, err = self.config.convertString(sv)
				} else {
					panic(fmt.Sprintf("Unexpected non-string value for backtick token: %#v", v.value))
				}
			} else {
				err = errFmt(nil, "Unable to evaluate '%#v'", v)
			}
		case []keyValue:
			result, err = self.config.wrapMapping(v)
		case Mapping:
			result = newMapWrapper(self.config, &v)
		case Sequence:
			result = newSeqWrapper(self.config, &v)
		case UnaryNode:
			switch v.op {
			case At:
				result, err = self.evalAt(v)
			case Dollar:
				result, err = self.getFromPath(v.operand)
			case Minus:
				var val Any

				val, err = self.evaluate(v.operand)
				if err == nil {
					result, err = self.negate(val)
				}
			default:
				err = errFmt(nil, "Unable to evaluate %#v", v)
			}
		case BinaryNode:
			var lhs Any

			lhs, err = self.evaluate(v.left)
			if err == nil {
				var lhsBool bool
				switch v.op {
				case And:
					lhsBool = lhs.(bool)
					if !lhsBool {
						result = false
					} else {
						result, err = self.evaluate(v.right)
						if err == nil {
							result = result.(bool)
						}
					}
				case Or:
					lhsBool = lhs.(bool)
					if lhsBool {
						result = true
					} else {
						result, err = self.evaluate(v.right)
						if err == nil {
							result = result.(bool)
						}
					}
				default:
					var rhs Any
					rhs, err = self.evaluate(v.right)
					if err == nil {
						switch v.op {
						case Plus:
							result, err = self.addValues(lhs, rhs)
						case Minus:
							result, err = self.subtractValues(lhs, rhs)
						case Star:
							result, err = self.multiplyValues(lhs, rhs)
						case Slash:
							result, err = self.divideValues(lhs, rhs)
						case SlashSlash:
							result, err = self.integerDivideValues(lhs, rhs)
						case Modulo:
							result, err = self.moduloValues(lhs, rhs)
						case LeftShift:
							result, err = self.leftShiftValues(lhs, rhs)
						case RightShift:
							result, err = self.rightShiftValues(lhs, rhs)
						case BitwiseOr:
							result, err = self.bitOrValues(lhs, rhs)
						case BitwiseAnd:
							result, err = self.bitAndValues(lhs, rhs)
						case BitwiseXor:
							result, err = self.bitXorValues(lhs, rhs)
						case Power:
							result, err = self.exponentiate(lhs, rhs)
						default:
							err = errFmt(nil, "Unable to evaluate %#v", v)
						}
					}
				}
			}
		default:
			err = errFmt(nil, "Unable to evaluate '%#v'", v)
		}
	}
	return result, err
}

func (self *evaluator) getSliceIndexOrStep(node Any, indexOrStep string) (int, error) {
	var result int
	var err error
	var item Any

	item, err = self.evaluate(node)
	if err == nil {
		if number, ok := item.(int64); ok {
			result = int(number)
		} else {
			err = errFmt(nil, "slice %s must be an integer, but is %s", item, indexOrStep)
		}
	}
	return result, err
}

func (self *evaluator) getSlice(seq SeqWrapper, node SliceNode) (SeqWrapper, error) {
	var result SeqWrapper
	var err error
	var startIndex int
	var stopIndex int
	var step int
	var size = len(*seq.data)

	if node.step == nil {
		step = 1
	} else {
		step, err = self.getSliceIndexOrStep(node.step, "step")
		if err == nil && step == 0 {
			err = errFmt(nil, "slice step cannot be zero")
		}
	}
	if node.start == nil {
		startIndex = 0
	} else {
		startIndex, err = self.getSliceIndexOrStep(node.start, "index")
		if err == nil {
			if startIndex < 0 {
				if startIndex >= -size {
					startIndex += size
				} else {
					startIndex = 0
				}
			} else if startIndex >= size {
				startIndex = size - 1
			}
		}
	}
	if err == nil {
		if node.stop == nil {
			stopIndex = size - 1
		} else {
			stopIndex, err = self.getSliceIndexOrStep(node.stop, "index")
			if err == nil {
				if stopIndex < 0 {
					if stopIndex >= -size {
						stopIndex += size
					} else {
						stopIndex = 0
					}
				}
				if stopIndex > size {
					stopIndex = size
				}
				if step < 0 {
					stopIndex++
				} else {
					stopIndex--
				}
			}
		}
	}
	if err == nil {
		if step < 0 && startIndex < stopIndex {
			tmp := stopIndex
			stopIndex = startIndex
			startIndex = tmp
		}
		items := make(Sequence, 0)

		i := startIndex
		var notDone bool

		if step > 0 {
			notDone = i <= stopIndex
		} else {
			notDone = i >= stopIndex
		}
		for notDone {
			items = append(items, (*seq.data)[i])
			i += step
			if step > 0 {
				notDone = i <= stopIndex
			} else {
				notDone = i >= stopIndex
			}
		}
		result = newSeqWrapper(self.config, &items)
	}
	return result, err
}

func isRef(node Any) bool {
	if un, ok := node.(UnaryNode); !ok {
		return false
	} else {
		return un.op == Dollar
	}
}

func (self *evaluator) getFromPath(node Any) (Any, error) {
	var result Any
	var err error

	pi := pathIterator(node)
	first := (<-pi).(Token).value.(string)
	result, err = self.config.Get(first)
	if err == nil {
		var currentEvaluator *evaluator
		var ok bool
		var cfg *Config

		if cfg, ok = result.(*Config); !ok {
			currentEvaluator = self
		} else {
			currentEvaluator = cfg.evaluator
		}
		for item := range pi {
			var operand Any
			var doEval = false

			pv := item.(pathValue)
			operand = pv.operand
			sn, sliced := operand.(SliceNode)
			if !sliced && pv.op != Dot {
				operand, err = currentEvaluator.evaluate(operand)
			}
			// do checks on operand types
			if sliced {
				if _, ok := result.(SeqWrapper); !ok {
					err = errFmt(nil, "slices can only operate on lists")
					break
				}
			} else {

			}
			switch rv := result.(type) {
			case *Config:
				currentEvaluator = cfg.evaluator
				if s, ok := operand.(string); ok {
					result, err = rv.Get(s)
				} else {
					err = errFmt(nil, "string required, but found %T(%v)", operand, operand)
				}
			case MapWrapper:
				if s, ok := operand.(string); ok {
					result, err = rv.baseGet(s)
				} else {
					err = errFmt(nil, "string required, but found %v", operand)
				}
				doEval = true
			case SeqWrapper:
				if sliced {
					result, err = self.getSlice(result.(SeqWrapper), sn)
				} else {
					if n, ok := operand.(int64); !ok {
						err = errFmt(nil, "integer required, but found '%v'", operand)
					} else {
						intN := int(n)
						size := len(*rv.data)
						if intN < 0 {
							if intN >= -size {
								intN += size
							}
						}
						if intN < 0 || intN >= size {
							err = errFmt(nil, "index out of range: is %d, must b between 0 and %d", intN, size-1)
						} else {
							result = rv.baseGet(intN)
							doEval = true
						}
					}
				}
			default:
				path := toSource(node)
				err = errFmt(nil, "Not found in configuration: %s", path)
			}
			if err != nil {
				break
			}
			if isRef(result) {
				un := result.(UnaryNode)
				if _, ok = (*currentEvaluator.refsSeen)[un]; ok {
					parts := make([]string, 0)
					for k := range *currentEvaluator.refsSeen {
						loc := k.operand.(BinaryNode).left.(Token).start
						s := fmt.Sprintf("%s %s", toSource(k.operand), loc)
						parts = append(parts, s)
					}
					if len(parts) > 1 {
						sort.Strings(parts)
					}
					err = errFmt(nil, "Circular reference: %s", strings.Join(parts, ", "))
					break
				}
				(*currentEvaluator.refsSeen)[un] = true
			}
			if doEval {
				result, err = currentEvaluator.evaluate(result)
			}
		}
	}
	return result, err
}

func NewConfig() *Config {
	var result = Config{true, true, make([]string, 0), "", "", nil, nil, nil,
		nil, nil, defaultStringConverter}
	var e = newEvaluator(&result)
	result.evaluator = &e
	return &result
}

func FromFile(path string) (*Config, error) {
	result := NewConfig()
	err := result.LoadFile(path)
	return result, err
}

/*func unwrap(o Any) (Any, error) {
	var result = o
	var err error

	switch v := o.(type) {
	case SeqWrapper:
		result, err = v.AsList()
	case MapWrapper:
		result, err = v.AsDict()
	}
	return result, err
}
*/

func (self *Config) setPath(path string) error {
	var err error

	path, err = filepath.Abs(path)
	if err == nil {
		self.Path = path
		if _, err = os.Stat(path); err == nil {
			self.RootDir = filepath.Dir(path)
		}
	}
	return err
}

func (self *Config) wrapMapping(items []keyValue) (MapWrapper, error) {
	var result MapWrapper
	var seen = make(map[string]Location)
	var err error
	var data = make(Mapping)

	for _, item := range items {
		var key = item.key.value.(string)
		if self.NoDuplicates {
			if _, ok := seen[key]; ok {
				err = errFmt(nil, "Duplicate key %v at %v (previously at %v)",
					key, item.key.start, seen[key])
				break
			}
			seen[key] = item.key.start
		}
		data[key] = item.value
	}
	if err == nil {
		result = newMapWrapper(self, &data)
	}
	return result, err
}

func (self *Config) convertString(s string) (Any, error) {
	var result Any
	var err error

	result = self.converter(s, self)
	if self.StrictConversions && (result == s) {
		err = errFmt(nil, "Unable to convert string %v", s)
	}
	return result, err
}

func (self *Config) Load(reader *io.Reader) error {
	parser, err := NewParser(reader)
	if err == nil {
		var node Any

		node, err = parser.Container()
		mb, ok := node.([]keyValue)
		if !ok {
			err = errFmt(nil, "Root configuration must be a mapping")
		} else {
			var data MapWrapper

			data, err = self.wrapMapping(mb)
			if err == nil {
				self.data = &data
				if self.cache != nil {
					var c = make(Mapping)
					self.cache = &c
				}
			}
		}
	}
	return err
}

func closeFile(file *os.File) {
	_ = file.Close()
}

func (self *Config) LoadFile(path string) error {
	var reader io.Reader

	f, err := os.Open(path)
	if err == nil {
		defer closeFile(f)
		reader = bufio.NewReader(f)
		err = self.Load(&reader)
		if err == nil {
			err = self.setPath(path)
		}
	}
	return err
}

func (self *Config) Get(key string) (Any, error) {
	var result Any
	var err error

	if self.data == nil {
		err = errFmt(nil, "No data in configuration")
	} else {
		if self.cache != nil {
			if r, ok := (*self.cache)[key]; ok {
				result = r
			}
		}
		if result == nil {
			result, err = self.data.Get(key)
		}
		if err == nil {
			if self.cache != nil {
				(*self.cache)[key] = result
			}
		}
	}
	//if err == nil {
	//	result, err = unwrap(result)
	//}
	return result, err
}

func (self *Config) GetWithDefault(key string, defaultValue Any) (Any, error) {
	result, err := self.Get(key)
	if err != nil {
		result = defaultValue
		err = nil
	}
	return result, err
}

func parsePath(s string) (Any, error) {
	var result Any
	var err error
	var parser Parser

	parser, err = makeParser(s)
	if err != nil {
		err = errFmt(&parser.next.start, "Invalid Path: %s", s)
	} else {
		var failed = false

		if parser.next.kind != Word {
			failed = true
		} else {
			var node Any

			node, err = parser.primary()
			if err == nil {
				if parser.atEnd() {
					result = node
				} else {
					failed = true
				}
			}
		}
		if failed {
			err = errFmt(&parser.next.start, "Invalid Path: %s", s)
		}
	}
	return result, err
}

type pathValue struct {
	op      tokenKind
	operand Any
}

func tokenValue(node Any) Any {
	if t, ok := node.(Token); ok {
		return t.value
	}
	panic(fmt.Sprintf("Token was expected, but got %#v", node))
}

func visit(node Any) <-chan Any {
	ch := make(chan Any)

	go func() {
		switch v := node.(type) {
		case Token:
			ch <- v
		case UnaryNode:
			for item := range visit(v.operand) {
				ch <- item
			}
		case BinaryNode:
			for item := range visit(v.left) {
				ch <- item
			}
			switch v.op {
			case Dot:
				ch <- pathValue{Dot, tokenValue(v.right)}
			case Colon:
				ch <- pathValue{Colon, v.right}
			default:
				ch <- pathValue{v.op, tokenValue(v.right)}
			}
		}
		close(ch)
	}()
	return ch
}

func pathIterator(start Any) <-chan Any {
	ch := make(chan Any)

	go func() {
		for item := range visit(start) {
			ch <- item
		}
		close(ch)
	}()
	return ch
}

func toSource(o Any) string {
	var result string

	switch v := o.(type) {
	case Token:
		result = fmt.Sprintf("%v", v.value)
	case BinaryNode:
		pi := pathIterator(v)
		parts := make([]string, 1)
		parts[0] = (<-pi).(Token).value.(string)
		for item := range pi {
			pv := item.(pathValue)
			switch pv.op {
			case Dot:
				parts = append(parts, ".")
				parts = append(parts, pv.operand.(string))
			case LeftBracket:
				parts = append(parts, "[")
				parts = append(parts, toSource(pv.operand))
				parts = append(parts, "]")
			case Colon:
				sn := pv.operand.(SliceNode)
				parts = append(parts, "[")
				if sn.start != nil {
					parts = append(parts, toSource(sn.start))
				}
				parts = append(parts, ":")
				if sn.stop != nil {
					parts = append(parts, toSource(sn.stop))
				}
				if sn.step != nil {
					parts = append(parts, ":")
					parts = append(parts, toSource(sn.step))
				}
				parts = append(parts, "]")
			}
		}
		result = strings.Join(parts, "")
	default:
		result = fmt.Sprintf("%v", v)
	}
	return result
}

func (self *Config) getFromPath(key string) (Any, error) {
	var result Any
	var err error

	if self.data == nil {
		err = errFmt(nil, "No data in configuration")
	} else {
		var node Any

		node, err = parsePath(key)
		if err == nil {
			rs := make(map[UnaryNode]bool)
			// clear out any existing refs
			self.evaluator.refsSeen = &rs
			result, err = self.evaluator.getFromPath(node)
		}
	}
	return result, err
}

func (self *Config) AsDict() (Mapping, error) {
	return self.data.AsDict()
}

func (self *Config) evaluated(v Any) (Any, error) {
	return self.evaluator.evaluate(v)
}
