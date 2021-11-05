/*
 * Copyright (C) 2019-2020 Red Dove Consultants Ltd. All rights reserved.
 */
package config

import (
	"fmt"
	"io"
	"reflect"
	"strings"
)

type Sequence []Any

type UnaryNode struct {
	op      tokenKind
	operand Any
}

func (u UnaryNode) String() string {
	return fmt.Sprintf("UnaryNode(%s, %v)", tokenDescriptions[u.op], u.operand)
}

type BinaryNode struct {
	op    tokenKind
	left  Any
	right Any
}

func (b BinaryNode) String() string {
	return fmt.Sprintf("BinaryNode(%s, %v, %v)", tokenDescriptions[b.op], b.left, b.right)
}

type SliceNode struct {
	start Any
	stop  Any
	step  Any
}

func (s SliceNode) String() string {
	return fmt.Sprintf("SliceNode(%v, %v, %v)", s.start, s.stop, s.step)
}

type keyValue struct {
	key   Token
	value Any
}

type Parser struct {
	tokenizer tokenizer
	next      Token
}

func NewParser(reader *io.Reader) (Parser, error) {
	var tokenizer = newTokenizer(reader)
	token, err := tokenizer.getToken()
	return Parser{tokenizer: tokenizer, next: token}, err
}

func makeParser(source string) (Parser, error) {
	var sr io.Reader = strings.NewReader(source)

	parser, err := NewParser(&sr)
	return parser, err
}

func (self *Parser) advance() (tokenKind, error) {
	token, err := self.tokenizer.getToken()
	if err == nil {
		self.next = token
		return token.kind, nil
	}
	return Error, err
}

func (self *Parser) expect(kind tokenKind) (Token, error) {
	var result Token
	var err error

	if self.next.kind == kind {
		result = self.next
		_, err = self.advance()
	} else {
		result.kind = Error
		err = errFmt(&self.next.start, "expected %v but got %v", tokenDescriptions[kind], tokenDescriptions[self.next.kind])
	}
	return result, err
}

func (self *Parser) consumeNewlines() (tokenKind, error) {
	var err error
	var result = self.next.kind

	for result == Newline && err == nil {
		result, err = self.advance()
	}
	return result, err
}

var expressionStarters = map[tokenKind]bool{
	LeftCurly:         true,
	LeftBracket:       true,
	LeftParenthesis:   true,
	At:                true,
	Dollar:            true,
	BackTick:          true,
	Plus:              true,
	Minus:             true,
	BitwiseComplement: true,
	Integer:           true,
	Float:             true,
	Complex:           true,
	True:              true,
	False:             true,
	None:              true,
	Not:               true,
	String:            true,
	Word:              true,
}

var valueStarters = map[tokenKind]bool{
	Integer:  true,
	Float:    true,
	Complex:  true,
	True:     true,
	False:    true,
	None:     true,
	String:   true,
	BackTick: true,
	Word:     true,
}

var comparisonOperators = map[tokenKind]bool{
	LessThan:           true,
	LessThanOrEqual:    true,
	GreaterThan:        true,
	GreaterThanOrEqual: true,
	Equal:              true,
	Unequal:            true,
	AltUnequal:         true,
	Is:                 true,
	In:                 true,
	Not:                true,
}

func (self *Parser) strings() (Token, error) {
	var result = self.next
	var err error
	var start = result.start
	var s = self.next.value.(string)
	var kind tokenKind

	kind, err = self.advance()
	if err == nil && kind == String {
		var end Location
		var text = make([]rune, 0)

		for err == nil && kind == String {
			text = append(text, []rune(s)...)
			s = self.next.value.(string)
			end = self.next.end
			kind, err = self.advance()
		}
		text = append(text, []rune(s)...)
		s = string(text)
		result.text = s
		result.value = s
		result.start.Update(&start)
		result.end.Update(&end)
	}
	return result, err
}

func (self *Parser) value() (Any, error) {
	var kind = self.next.kind
	var result Any
	var err error
	var token Token

	if _, ok := valueStarters[kind]; !ok {
		err = errFmt(&self.next.start, "unexpected when looking for value: %v", tokenDescriptions[kind])
	} else {
		if kind == String {
			token, err = self.strings()
		} else {
			token = self.next
			_, err = self.advance()
		}
	}
	if err == nil {
		result = token
	}
	return result, err
}

func (self *Parser) atEnd() bool {
	return self.next.kind == EOF
}

func (self *Parser) listBody() (Any, error) {
	var result = make(Sequence, 0)
	var err error
	var kind tokenKind
	var value Any

	kind, err = self.consumeNewlines()
	if err == nil {
		for err == nil && expressionStarters[kind] {
			value, err = self.Expr()

			if err == nil {
				result = append(result, value)
				kind = self.next.kind
				if kind != Comma && kind != Newline {
					break
				}
				_, err = self.advance()
				if err == nil {
					kind, err = self.consumeNewlines()
				}
			}
		}
	}
	return result, err
}

func (self *Parser) list() (Any, error) {
	var result Any
	var err error

	_, err = self.expect(LeftBracket)
	if err == nil {
		result, err = self.listBody()
		if err == nil {
			_, err = self.expect(RightBracket)
		}
	}
	return result, err
}

func (self *Parser) mapping() (Any, error) {
	var result Any
	var err error

	_, err = self.expect(LeftCurly)
	if err == nil {
		result, err = self.MappingBody()
		if err == nil {
			_, err = self.expect(RightCurly)
		}
	}
	return result, err
}

func (self *Parser) Container() (Any, error) {
	var result Any
	var err error
	var kind tokenKind

	kind, err = self.consumeNewlines()

	if err == nil {
		if kind == LeftCurly {
			result, err = self.mapping()
		} else if kind == LeftBracket {
			result, err = self.list()
		} else if kind == Word || kind == String {
			result, err = self.MappingBody()
		} else {
			err = errFmt(&self.next.start, "unexpected while looking for container: %v", tokenDescriptions[kind])
		}
		if err == nil {
			_, err = self.consumeNewlines()
		}
	}
	return result, err
}

func (self *Parser) atom() (Any, error) {
	var result Any
	var err error
	var kind = self.next.kind

	switch kind {
	case LeftCurly:
		result, err = self.mapping()
	case LeftBracket:
		result, err = self.list()
	case Dollar:
		_, err = self.expect(Dollar)
		if err == nil {
			_, err = self.expect(LeftCurly)
			if err == nil {
				result, err = self.primary()
				if err == nil {
					result = UnaryNode{Dollar, result}
					_, err = self.expect(RightCurly)
				}
			}
		}
	case Word, Integer, Float, Complex, String, BackTick, True, False, None:
		result, err = self.value()
	case LeftParenthesis:
		_, err = self.expect(LeftParenthesis)
		if err == nil {
			result, err = self.Expr()
			if err == nil {
				_, err = self.expect(RightParenthesis)
			}
		}
	default:
		err = errFmt(&self.next.start, "unexpected: %v", tokenDescriptions[kind])
	}
	return result, err
}

func (self *Parser) compOp() (tokenKind, error) {
	var result = self.next.kind
	var err error

	_, err = self.advance()
	if err == nil {
		var shouldAdvance = false

		if result == Is && self.next.kind == Not {
			result = IsNot
			shouldAdvance = true
		} else if result == Not && self.next.kind == In {
			result = NotIn
			shouldAdvance = true
		}
		if shouldAdvance {
			_, err = self.advance()
		}
	}
	return result, err
}

func invalidIndex(n int, loc *Location) error {
	return errFmt(loc, "invalid index: expected 1 expression, found %d", n)
}

func (self *Parser) trailer() (tokenKind, Any, error) {
	var kind = self.next.kind
	var result Any
	var err error

	if kind != LeftBracket {
		_, err = self.expect(Dot)
		if err == nil {
			result, err = self.expect(Word)
		}
	} else {
		kind, err = self.advance()
		if err == nil {
			var isSlice = false
			var startIndex Any
			var stopIndex Any
			var step Any
			var node Any

			// TODO factor out common code

			if kind == Colon {
				isSlice = true
			} else {
				node, err = self.listBody()
				if err == nil {
					lb, ok := node.(Sequence)
					if !ok {
						// TODO raise an error
					} else {
						var n = len(lb)

						if n != 1 {
							err = invalidIndex(n, &self.next.start)
						} else {
							kind = self.next.kind
							if kind != Colon {
								result = lb[0]
								kind = LeftBracket
							} else {
								startIndex = lb[0]
								isSlice = true
							}
						}
					}
				}
			}
			if err == nil {
				if isSlice {
					kind, err = self.advance()
					if err == nil {
						if kind == Colon {
							// no stop, but there might be a step
							kind, err = self.advance()
							if err == nil && kind != RightBracket {
								node, err = self.listBody()
								if err == nil {
									lb, ok := node.(Sequence)
									if !ok {
										// TODO raise an error
									} else {
										var n = len(lb)

										if n != 1 {
											err = invalidIndex(n, &self.next.start)
										} else {
											step = lb[0]
										}
									}
								}
							}
						} else if kind != RightBracket {
							node, err = self.listBody()
							if err == nil {
								lb, ok := node.(Sequence)
								if !ok {
									// TODO raise an error
								} else {
									var n = len(lb)

									if n != 1 {
										err = invalidIndex(n, &self.next.start)
									} else {
										stopIndex = lb[0]
										kind = self.next.kind
										if kind == Colon {
											kind, err = self.advance()
											if err == nil && kind != RightBracket {
												node, err = self.listBody()
												if err == nil {
													lb, ok = node.(Sequence)
													if !ok {
														// TODO raise an error
													} else {
														var n = len(lb)

														if n != 1 {
															err = invalidIndex(n, &self.next.start)
														} else {
															step = lb[0]
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
					if err == nil {
						kind = Colon
						result = SliceNode{startIndex, stopIndex, step}
					}
				}
				if err == nil {
					_, err = self.expect(RightBracket)
				}
			}
		}
	}
	return kind, result, err
}

func (self *Parser) primary() (Any, error) {
	var result Any
	var err error

	result, err = self.atom()
	for err == nil && (self.next.kind == Dot || self.next.kind == LeftBracket) {
		op, rhs, e := self.trailer()

		if e == nil {
			result = BinaryNode{op, result, rhs}
		} else {
			err = e
		}
	}
	return result, err
}

func (self *Parser) power() (Any, error) {
	var result Any
	var err error

	result, err = self.primary()

	for err == nil && self.next.kind == Power {
		var rhs Any

		_, err = self.advance()
		rhs, err = self.unaryExpr()
		if err == nil {
			result = BinaryNode{Power, result, rhs}
		}
	}
	return result, err
}

func (self *Parser) unaryExpr() (Any, error) {
	var result Any
	var err error
	var kind = self.next.kind

	if kind != Plus && kind != Minus && kind != BitwiseComplement && kind != At {
		result, err = self.power()
	} else {
		var rhs Any

		_, err = self.advance()
		rhs, err = self.unaryExpr()
		if err == nil {
			result = UnaryNode{op: kind, operand: rhs}
		}
	}
	return result, err
}

func (self *Parser) mulExpr() (Any, error) {
	var result Any
	var err error

	result, err = self.unaryExpr()

	for err == nil && (self.next.kind == Star || self.next.kind == Slash || self.next.kind == SlashSlash || self.next.kind == Modulo) {
		var rhs Any
		var op = self.next.kind

		_, err = self.advance()
		rhs, err = self.unaryExpr()
		if err == nil {
			result = BinaryNode{op, result, rhs}
		}
	}
	return result, err
}

func (self *Parser) addExpr() (Any, error) {
	var result Any
	var err error

	result, err = self.mulExpr()

	for err == nil && (self.next.kind == Plus || self.next.kind == Minus) {
		var rhs Any
		var op = self.next.kind

		_, err = self.advance()
		rhs, err = self.mulExpr()
		if err == nil {
			result = BinaryNode{op, result, rhs}
		}
	}
	return result, err
}

func (self *Parser) shiftExpr() (Any, error) {
	var result Any
	var err error

	result, err = self.addExpr()

	for err == nil && (self.next.kind == LeftShift || self.next.kind == RightShift) {
		var rhs Any
		var op = self.next.kind

		_, err = self.advance()
		rhs, err = self.addExpr()
		if err == nil {
			result = BinaryNode{op, result, rhs}
		}
	}
	return result, err
}

func (self *Parser) bitAndExpr() (Any, error) {
	var result Any
	var err error

	result, err = self.shiftExpr()

	for err == nil && self.next.kind == BitwiseAnd {
		var rhs Any

		_, err = self.advance()
		rhs, err = self.shiftExpr()
		if err == nil {
			result = BinaryNode{BitwiseAnd, result, rhs}
		}
	}
	return result, err
}

func (self *Parser) bitXorExpr() (Any, error) {
	var result Any
	var err error

	result, err = self.bitAndExpr()

	for err == nil && self.next.kind == BitwiseXor {
		var rhs Any

		_, err = self.advance()
		rhs, err = self.bitAndExpr()
		if err == nil {
			result = BinaryNode{BitwiseXor, result, rhs}
		}
	}
	return result, err
}

func (self *Parser) bitOrExpr() (Any, error) {
	var result Any
	var err error

	result, err = self.bitXorExpr()

	for err == nil && self.next.kind == BitwiseOr {
		var rhs Any

		_, err = self.advance()
		rhs, err = self.bitXorExpr()
		if err == nil {
			result = BinaryNode{BitwiseOr, result, rhs}
		}
	}
	return result, err
}

func (self *Parser) comparison() (Any, error) {
	var result Any
	var err error

	result, err = self.bitOrExpr()

	if err == nil && comparisonOperators[self.next.kind] {
		var op tokenKind

		op, err = self.compOp()
		if err == nil {
			var rhs Any

			rhs, err = self.bitOrExpr()
			if err == nil {
				result = BinaryNode{op, result, rhs}
			}
		}
	}
	return result, err
}

func (self *Parser) notExpr() (Any, error) {
	var result Any
	var err error

	if self.next.kind != Not {
		result, err = self.comparison()
	} else {
		var rhs Any

		_, err = self.advance()
		rhs, err = self.notExpr()
		if err == nil {
			result = UnaryNode{op: Not, operand: rhs}
		}
	}
	return result, err
}

func (self *Parser) andExpr() (Any, error) {
	var result Any
	var err error

	result, err = self.notExpr()

	for err == nil && self.next.kind == And {
		var rhs Any

		_, err = self.advance()
		rhs, err = self.notExpr()
		if err == nil {
			result = BinaryNode{And, result, rhs}
		}
	}
	return result, err
}

func (self *Parser) Expr() (Any, error) {
	var result Any
	var err error

	result, err = self.andExpr()

	for err == nil && self.next.kind == Or {
		var rhs Any

		_, err = self.advance()
		rhs, err = self.andExpr()
		if err == nil {
			result = BinaryNode{Or, result, rhs}
		}
	}
	return result, err
}

func (self *Parser) objectKey() (Token, error) {
	var result Token
	var err error
	var kind = self.next.kind

	if kind == String {
		result, err = self.strings()
	} else {
		result = self.next
		_, err = self.advance()
	}
	return result, err
}

func (self *Parser) MappingBody() (Any, error) {
	var result = make([]keyValue, 0)
	var err error
	var kind tokenKind
	var value Any

	kind, err = self.consumeNewlines()
	if err == nil {
		if kind != RightCurly && kind != EOF {
			if kind != Word && kind != String {
				err = errFmt(&self.next.start, "unexpected while looking for key: %v", tokenDescriptions[kind])
			} else {
				for err == nil && (kind == Word || kind == String) {
					var key Token

					key, err = self.objectKey()
					if err == nil {
						kind = self.next.kind
						if kind == Colon || kind == Assign {
							_, err = self.advance()
						}
						if err == nil {
							_, err = self.consumeNewlines()
						}
						if err == nil {
							value, err = self.Expr()
							if err == nil {
								kv := keyValue{key: key, value: value}
								result = append(result, kv)
								kind = self.next.kind
								if kind == Comma || kind == Newline {
									_, err = self.advance()
									if err == nil {
										kind, err = self.consumeNewlines()
									}
								} else if kind != RightCurly && kind != EOF {
									err = errFmt(&self.next.start, "unexpected after key-value pair: %v", tokenDescriptions[kind])
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

func ParseRule(source string, rule string) (Any, error) {
	var result Any
	var ok bool

	parser, err := makeParser(source)
	if err != nil {
		return nil, err
	} else {
		method := reflect.ValueOf(&parser).MethodByName(rule)
		if !method.IsValid() {
			err = fmt.Errorf("no such rule: %v", rule)
		} else {
			callResult := method.Call([]reflect.Value{})
			// there should be two elements - result and error
			result = callResult[0].Interface()
			e := callResult[1].Interface()

			if e != nil {
				err, ok = e.(*RecognizerError)
				if !ok {
					panic(e)
				} else {

				}
			}
		}
	}
	return result, err
}

func Parse(source string) (Any, error) {
	node, err := ParseRule(source, "MappingBody")
	return node, err
}
