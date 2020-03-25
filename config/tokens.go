/*
 * Copyright (C) 2019-2020 Red Dove Consultants Ltd. All rights reserved.
 */
package config

import (
	"bufio"
	"fmt"
	"io"
	"reflect"
	"strconv"
	"strings"
	"unicode"
)

type tokenKind int

const (
	EOF tokenKind = iota
	Word
	Integer
	Float
	String
	Newline
	LeftCurly
	RightCurly
	LeftBracket
	RightBracket
	LeftParenthesis
	RightParenthesis
	LessThan
	GreaterThan
	LessThanOrEqual
	GreaterThanOrEqual
	Assign
	Equal
	Unequal
	AltUnequal
	LeftShift
	RightShift
	Dot
	Comma
	Colon
	At
	Plus
	Minus
	Star
	Power
	Slash
	SlashSlash
	Modulo
	BackTick
	Dollar
	True
	False
	None
	Is
	In
	Not
	And
	Or
	BitwiseAnd
	BitwiseOr
	BitwiseXor
	BitwiseComplement
	Complex
	IsNot
	NotIn
	Error
)

type Any interface{}

type missing struct {
}

type Location struct {
	line   int
	column int
}

type RecognizerError struct {
	message  string
	Location *Location
}

type Token struct {
	kind  tokenKind
	text  string
	value Any
	start Location
	end   Location
}

type pushBack struct {
	char rune
	pos  Location
}

type tokenizer struct {
	reader       *bufio.Reader
	pushedBack   []pushBack
	location     Location
	charLocation Location
}

func newLocation() Location {
	result := Location{line: 1, column: 1}
	return result
}

func copyLocation(other *Location) Location {
	return Location{line: other.line, column: other.column}
}

func (self *Location) Update(other *Location) {
	self.line = other.line
	self.column = other.column
}

func (self *Location) NextLine() {
	self.line++
	self.column = 1
}

func (loc Location) String() string {
	return fmt.Sprintf("(%v, %v)", loc.line, loc.column)
}

func (e RecognizerError) Error() string {
	return e.message
}

func errFmt(loc *Location, format string, a ...interface{}) *RecognizerError {
	if loc != nil {
		copied := copyLocation(loc)
		loc = &copied
	}
	return &RecognizerError{message: fmt.Sprintf(format, a...), Location: loc}
}

func newToken(kind tokenKind, text string, value Any) Token {
	result := Token{kind: kind, text: text, value: value}

	result.start = newLocation()
	result.end = newLocation()
	return result
}

func isInstanceOf(unknown, known interface{}) bool {
	return reflect.TypeOf(unknown) == reflect.TypeOf(known)
}

var tokenDescriptions = map[tokenKind]string{
	EOF:                "EOF",
	Word:               "Word",
	Integer:            "Integer",
	Float:              "Float",
	String:             "String",
	Newline:            "Newline",
	LeftCurly:          "'{'",
	RightCurly:         "'}'",
	LeftBracket:        "'['",
	RightBracket:       "']'",
	LeftParenthesis:    "'('",
	RightParenthesis:   "')'",
	LessThan:           "'<'",
	GreaterThan:        "'>'",
	LessThanOrEqual:    "'<='",
	GreaterThanOrEqual: "'>='",
	Assign:             "=",
	Equal:              "'=='",
	Unequal:            "'!='",
	AltUnequal:         "'<>'",
	LeftShift:          "'<<'",
	RightShift:         "'>>'",
	Dot:                "'.'",
	Comma:              "','",
	Colon:              "':'",
	At:                 "'@'",
	Plus:               "'+'",
	Minus:              "'-'",
	Star:               "'*'",
	Power:              "'**'",
	Slash:              "'/'",
	SlashSlash:         "'//'",
	Modulo:             "'%'",
	BackTick:           "'`'",
	Dollar:             "'$'",
	True:               "'true'",
	False:              "'false'",
	None:               "'null'",
	Is:                 "'is'",
	In:                 "'in'",
	Not:                "'not'",
	And:                "'and'",
	Or:                 "'or'",
	BitwiseAnd:         "'&'",
	BitwiseOr:          "'|'",
	BitwiseXor:         "'^'",
	BitwiseComplement:  "'~'",
	Complex:            "'complex'",
	IsNot:              "'is not'",
	NotIn:              "'not in'",
	Error:              "'error'",
}

func (t Token) String() string {
	return fmt.Sprintf("Token(%s:%s:%v)", tokenDescriptions[t.kind], t.text, t.value)
}

var punctuation = map[rune]tokenKind{
	':': Colon,
	'-': Minus,
	'+': Plus,
	'*': Star,
	'/': Slash,
	'%': Modulo,
	',': Comma,
	'{': LeftCurly,
	'}': RightCurly,
	'[': LeftBracket,
	']': RightBracket,
	'(': LeftParenthesis,
	')': RightParenthesis,
	'@': At,
	'$': Dollar,
	'<': LessThan,
	'>': GreaterThan,
	'!': Not,
	'~': BitwiseComplement,
	'&': BitwiseAnd,
	'|': BitwiseOr,
	'^': BitwiseXor,
	'.': Dot,
}

var keywords = map[string]tokenKind{
	"true":  True,
	"false": False,
	"null":  None,
	"is":    Is,
	"in":    In,
	"not":   Not,
	"and":   And,
	"or":    Or,
}

var NullValue = missing{}

var keywordValues = map[tokenKind]Any{
	True:  true,
	False: false,
	None:  NullValue,
}

func newTokenizer(reader *io.Reader) tokenizer {
	var bufferedReader *bufio.Reader

	if isInstanceOf(reader, (*bufio.Reader)(nil)) {
		var temp interface{} = reader
		bufferedReader = temp.(*bufio.Reader)
	} else {
		bufferedReader = bufio.NewReader(*reader)
	}
	result := tokenizer{reader: bufferedReader, pushedBack: []pushBack{}}
	result.charLocation = newLocation()
	result.location = newLocation()
	return result
}

func (self *tokenizer) pushBack(c rune) {
	if (c != 0) && ((c == '\n') || !unicode.IsSpace(c)) {
		pb := pushBack{char: c, pos: copyLocation(&self.charLocation)}
		self.pushedBack = append(self.pushedBack, pb)
	}
}

func (self *tokenizer) getChar() rune {
	var result rune

	if len(self.pushedBack) > 0 {
		var n = len(self.pushedBack)
		var pb = self.pushedBack[n-1]

		self.pushedBack = self.pushedBack[:n-1]
		self.charLocation.Update(&pb.pos)
		self.location.Update(&pb.pos)
		result = pb.char
	} else {
		c, size, err := self.reader.ReadRune()
		if err != nil {
			// TODO bubble up
		} else if result == 0xFFFD && size == 1 {
			// TODO wrong encoding
		}
		self.charLocation.Update(&self.location)
		result = c
	}
	if result != 0 {
		self.location.column++
		if result == '\n' {
			self.location.NextLine()
		}
	}
	return result
}

func indexOf(text []rune, c rune) int {
	var result = -1

	for i, sc := range text {
		if sc == c {
			result = i
			break
		}
	}
	return result
}

func isHexDigit(c rune) bool {
	return unicode.IsDigit(c) || ((c >= 'a') && (c <= 'f')) || ((c >= 'A') && (c <= 'F'))
}

func (self *tokenizer) appendChar(text []rune, c rune, endLocation *Location) []rune {
	text = append(text, c)
	endLocation.Update(&self.charLocation)
	return text
}

func (self *tokenizer) getNumber(text []rune, startLocation Location, endLocation *Location) ([]rune, tokenKind, Any, error) {
	var kind = Integer
	var value Any = nil
	var inExponent = false
	var radix = 0
	var dotSeen = indexOf(text, '.') >= 0
	var lastWasDigit = unicode.IsDigit(text[len(text)-1])
	var err error
	var c rune

	for true {
		c = self.getChar()

		if c == 0 {
			break
		}
		if c == '.' {
			dotSeen = true
		}
		if c == '_' {
			if lastWasDigit {
				text = self.appendChar(text, c, endLocation)
				lastWasDigit = false
				continue
			} else {
				text = self.appendChar(text, c, endLocation)
				err = errFmt(&self.charLocation, "invalid '_' in number: %v", string(text))
				break
			}
		}
		lastWasDigit = false // unless set in one of the clauses below
		if (radix == 0) && (c >= '0') && (c <= '9') {
			text = self.appendChar(text, c, endLocation)
			lastWasDigit = true
		} else if (radix == 2) && (c >= '0') && (c <= '1') {
			text = self.appendChar(text, c, endLocation)
			lastWasDigit = true
		} else if (radix == 8) && (c >= '0') && (c <= '7') {
			text = self.appendChar(text, c, endLocation)
			lastWasDigit = true
		} else if (radix == 16) && isHexDigit(c) {
			text = self.appendChar(text, c, endLocation)
			lastWasDigit = true
		} else if ((c == 'o') || (c == 'O') ||
			(c == 'x') || (c == 'X') || (c == 'b') || (c == 'B')) &&
			(len(text) == 1) && (text[0] == '0') {
			if (c == 'o') || (c == 'O') {
				radix = 8
			} else if (c == 'x') || (c == 'X') {
				radix = 16
			} else {
				radix = 2
			}
			text = self.appendChar(text, c, endLocation)
		} else if (radix == 0) && (c == '.') && !inExponent && (indexOf(text, c) < 0) {
			text = self.appendChar(text, c, endLocation)
		} else if (radix == 0) && (c == '-') && (indexOf(text[1:], '-') < 0) && inExponent {
			text = self.appendChar(text, c, endLocation)
		} else if (radix == 0) &&
			((c == 'e') || (c == 'E')) &&
			(indexOf(text, 'e') < 0) &&
			(indexOf(text, 'E') < 0) && text[len(text)-1] != '_' {
			text = self.appendChar(text, c, endLocation)
			inExponent = true
		} else {
			break
		}
	}
	if err == nil {
		if text[len(text)-1] == '_' {
			err = errFmt(&self.charLocation, "invalid '_' at end of number: %v", string(text))
		} else if (radix == 0) && ((c == 'j') || (c == 'J')) {
			text = self.appendChar(text, c, endLocation)
			kind = Complex
		} else {
			// not allowed to have a letter or digit which wasn't accepted
			if c != '.' && !(unicode.IsDigit(c) || unicode.IsLetter(c)) {
				self.pushBack(c)
			} else {
				err = errFmt(&self.charLocation, "unexpected character in number: %c", c)
			}
		}
		if err == nil {
			// collected chars, now compute value etc.
			var s = strings.Replace(string(text), "_", "", -1)
			var fv float64

			if radix != 0 {
				value, err = strconv.ParseInt(s[2:], radix, 64)
			} else if kind == Complex {
				fv, err = strconv.ParseFloat(s[:len(s)-1], 64)
				if err == nil {
					value = complex(0, fv)
				}
			} else {
				if dotSeen || inExponent {
					kind = Float
					fv, err = strconv.ParseFloat(s, 64)
					if err == nil {
						value = fv
					}
				} else {
					if s[0] == '0' {
						radix = 8
					} else {
						radix = 10
					}
					var uv int64

					uv, err = strconv.ParseInt(s, radix, 64)
					if err == nil {
						value = uv
					} else {
						err = errFmt(&startLocation, "badly-formed number: '%v'", s)
					}
				}
			}
		}
	}
	return text, kind, value, err
}

var escapes = map[rune]rune{
	'a':  '\a',
	'b':  '\b',
	'f':  '\f',
	'n':  '\n',
	'r':  '\r',
	't':  '\t',
	'v':  '\v',
	'\\': '\\',
	'\'': '\'',
	'"':  '"',
}

func parseEscapes(s []rune, loc *Location) ([]rune, error) {
	var result []rune
	var err error = nil

	i := indexOf(s, '\\')
	if i < 0 {
		result = s
	} else {
		var text = make([]rune, 0)
		var failed = false

		for i >= 0 {
			n := len(s)
			if i > 0 {
				text = append(text, s[0:i]...)
			}
			c := s[i+1]
			if replacement, ok := escapes[c]; ok {
				text = append(text, replacement)
				i += 2
			} else if c == 'x' || c == 'X' || c == 'u' || c == 'U' {
				var sLen int

				if c == 'x' || c == 'X' {
					sLen = 4
				} else if c == 'u' {
					sLen = 6
				} else {
					sLen = 10
				}
				if (i + sLen) > n {
					failed = true
					break
				} else {
					p := string(s[i+2 : i+sLen])
					v, err := strconv.ParseInt(p, 16, 64)
					if err != nil {
						failed = true
						break
					} else if (v >= 0xd800 && v < 0xe000) || (v >= 0x110000) {
						failed = true
						break
					} else {
						text = append(text, rune(v))
						i += sLen
					}
				}
			} else {
				failed = true
				break
			}
			s = s[i:]
			i = indexOf(s, '\\')
		}
		if failed {
			err = errFmt(loc, "invalid escape sequence at index %d", i)
		} else {
			text = append(text, s...)
			result = text
		}
	}
	return result, err
}

func (self *tokenizer) getToken() (Token, error) {
	var t = newToken(Error, "", nil)
	var text = make([]rune, 0)
	var err error = nil
	var startLocation = newLocation()
	var endLocation = newLocation()

	for true {
		var c = self.getChar()

		startLocation.Update(&self.charLocation)
		endLocation.Update(&self.charLocation)

		if c == 0 {
			t.kind = EOF
			break
		}
		if c == '#' {
			var s, _ = self.reader.ReadString('\n')

			text = append(text, c)
			text = append(text, []rune(s)...)
			t.kind = Newline
			self.location.NextLine()
			endLocation.Update(&self.location)
			break
		} else if c == '\n' {
			text = self.appendChar(text, c, &endLocation)
			endLocation.Update(&self.location)
			t.kind = Newline
			break
		} else if c == '\\' {
			c = self.getChar()
			if c == '\n' {
				endLocation.Update(&self.location)
				continue
			} else {
				err = errFmt(&self.charLocation, "unexpected character: \\")
				break
			}
		} else if unicode.IsSpace(c) {
			continue
		} else if unicode.IsLetter(c) || (c == '_') {
			t.kind = Word
			text = self.appendChar(text, c, &endLocation)
			c = self.getChar()
			for c != 0 && (unicode.IsLetter(c) || unicode.IsDigit(c) || (c == '_')) {
				text = self.appendChar(text, c, &endLocation)
				c = self.getChar()
			}
			self.pushBack(c)
			var s = string(text)
			t.value = s
			if val, ok := keywords[s]; ok {
				t.kind = val
				if kv, ok := keywordValues[val]; ok {
					t.value = kv
				} else {
					t.value = nil
				}
			}
			break
		} else if c == '`' {
			t.kind = BackTick
			text = self.appendChar(text, c, &endLocation)
			for true {
				c = self.getChar()
				if c == 0 {
					break
				}
				text = self.appendChar(text, c, &endLocation)
				if c == '`' {
					break
				}
			}
			if c == 0 {
				err = errFmt(&startLocation, "unterminated `-string: %s", string(text))
			} else {
				var v []rune

				t.text = string(text)
				v, err = parseEscapes(text[1:len(text)-1], &startLocation)
				if err == nil {
					t.value = string(v)
				}
			}
			break
		} else if c == '\'' || c == '"' {
			var n int
			quote := c
			multiLine := false
			escaped := false

			t.kind = String
			text = self.appendChar(text, c, &endLocation)

			c1 := self.getChar()
			c1Loc := copyLocation(&self.charLocation)

			if c1 != quote {
				self.pushBack(c1)
			} else {
				c2 := self.getChar()
				if c2 != quote {
					self.pushBack(c2)
					if c2 == 0 {
						self.charLocation.Update(&c1Loc)
					}
					self.pushBack(c1)
				} else {
					multiLine = true
					text = append(text, quote)
					text = self.appendChar(text, quote, &endLocation)
				}
			}

			quoter := string(text)

			for true {
				c = self.getChar()
				if c == 0 {
					break
				}
				text = self.appendChar(text, c, &endLocation)
				if c == quote && !escaped {
					n = len(text)
					if !multiLine || (n >= 6) && string(text[n-3:n]) == quoter && text[n-4] != '\\' {
						break
					}
				}
				if c == '\\' {
					escaped = !escaped
				} else {
					escaped = false
				}
			}
			if c == 0 {
				err = errFmt(&startLocation, "unterminated string: %s", string(text))
			} else {
				var v []rune

				t.text = string(text)
				v, err = parseEscapes(text[len(quoter):len(text)-len(quoter)], &startLocation)
				if err == nil {
					t.value = string(v)
				}
			}
			break
		} else if unicode.IsDigit(c) {
			text = self.appendChar(text, c, &endLocation)
			text, t.kind, t.value, err = self.getNumber(text, startLocation, &endLocation)
			break
		} else if c == '=' {
			nc := self.getChar()
			if nc != c {
				t.kind = Assign
				text = append(text, c)
				self.pushBack(nc)
			} else {
				t.kind = Equal
				text = append(text, c)
				text = self.appendChar(text, c, &endLocation)
			}
			break
		} else if pv, ok := punctuation[c]; ok {
			t.kind = pv
			text = self.appendChar(text, c, &endLocation)
			if c == '.' {
				c = self.getChar()
				if !unicode.IsDigit(c) {
					self.pushBack(c)
				} else {
					text = self.appendChar(text, c, &endLocation)
					text, t.kind, t.value, err = self.getNumber(text, startLocation, &endLocation)
				}
			} else if c == '-' {
				c = self.getChar()
				if !unicode.IsDigit(c) && (c != '.') {
					self.pushBack(c)
				} else {
					text = self.appendChar(text, c, &endLocation)
					text, t.kind, t.value, err = self.getNumber(text, startLocation, &endLocation)
				}
			} else if c == '<' {
				c = self.getChar()
				if c == '=' {
					t.kind = LessThanOrEqual
					text = self.appendChar(text, c, &endLocation)
				} else if c == '>' {
					t.kind = AltUnequal
					text = self.appendChar(text, c, &endLocation)
				} else if c == '<' {
					t.kind = LeftShift
					text = self.appendChar(text, c, &endLocation)
				} else {
					self.pushBack(c)
				}
			} else if c == '>' {
				c = self.getChar()
				if c == '=' {
					t.kind = GreaterThanOrEqual
					text = self.appendChar(text, c, &endLocation)
				} else if c == '>' {
					t.kind = RightShift
					text = self.appendChar(text, c, &endLocation)
				} else {
					self.pushBack(c)
				}
			} else if c == '!' {
				c = self.getChar()
				if c == '=' {
					t.kind = Unequal
					text = self.appendChar(text, c, &endLocation)
				} else {
					self.pushBack(c)
				}
			} else if c == '/' {
				c = self.getChar()
				if c != '/' {
					self.pushBack(c)
				} else {
					t.kind = SlashSlash
					text = self.appendChar(text, c, &endLocation)
				}
			} else if c == '*' {
				c = self.getChar()
				if c != '*' {
					self.pushBack(c)
				} else {
					t.kind = Power
					text = self.appendChar(text, c, &endLocation)
				}
			} else if c == '&' || c == '|' {
				c2 := self.getChar()
				if c2 != c {
					self.pushBack(c2)
				} else {
					text = self.appendChar(text, c, &endLocation)
					if c2 == '&' {
						t.kind = And
					} else {
						t.kind = Or
					}
				}
			}
			break
		} else {
			err = errFmt(&self.charLocation, "unexpected character: %c", c)
			break
		}
	}
	if err != nil {
		t.kind = Error
	} else {
		t.text = string(text)
		t.start.Update(&startLocation)
		t.end.Update(&endLocation)
	}
	return t, err
}
