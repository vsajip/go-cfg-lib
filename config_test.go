/*
 * Copyright (C) 2019-2020 Red Dove Consultants Ltd. All rights reserved.
 *
 */
package config

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

var separator = regexp.MustCompile(`^-- ([A-Z]\d+) -+`)

const datetimeLayout = "2006-01-02T15:04:05.999999Z07:00:00.999999"

func loadData(filename string) (map[string]string, error) {
	var result = make(map[string]string)

	file, err := os.Open(filename)
	if err == nil {
		var scanner = bufio.NewScanner(file)
		var key *string
		var parts = make([]string, 0)

		defer closeFile(file)
		key = nil
		for scanner.Scan() {
			var line = scanner.Text()
			var matches = separator.FindStringSubmatch(line)
			if matches == nil {
				parts = append(parts, line)
			} else {
				if key != nil && len(parts) > 0 {
					result[*key] = strings.Join(parts, "\n")
				}
				key = &matches[1]
				parts = make([]string, 0)
			}
		}
	}

	return result, err
}

func TestLocation(t *testing.T) {
	loc := newLocation()
	assert.Equal(t, 1, loc.line)
	assert.Equal(t, 1, loc.column)

	loc2 := newLocation()
	loc2.line = 2
	loc2.column = 2
	loc.Update(&loc2)

	assert.Equal(t, 2, loc.line)
	assert.Equal(t, 2, loc.line)
}

func checkLocation(t *testing.T, loc Location, line int, column int) {
	assert.Equal(t, line, loc.line)
	assert.Equal(t, column, loc.column)
}

func checkTokenizerGetChar(t *testing.T, reader *io.Reader, expected string) {
	var tokenizer = newTokenizer(reader)

	assert.Len(t, tokenizer.pushedBack, 0)
	checkLocation(t, tokenizer.charLocation, 1, 1)
	checkLocation(t, tokenizer.location, 1, 1)

	var chars = make([]rune, 0)

	for true {
		c := tokenizer.getChar()
		if c == 0 {
			break
		}
		chars = append(chars, c)
	}
	var s = string(chars)
	assert.Equal(t, expected, s)
}

func TestTokenizerPushBack(t *testing.T) {
	var source = "{foo: bar}"
	var sr io.Reader = strings.NewReader(source)
	var tokenizer = newTokenizer(&sr)
	var c = tokenizer.getChar()

	assert.Equal(t, '{', c)
	line := tokenizer.charLocation.line
	column := tokenizer.charLocation.column

	tokenizer.pushBack(c)
	tokenizer.charLocation.line = 100
	tokenizer.charLocation.column = 100
	tokenizer.location.line = 200
	tokenizer.location.column = 200

	c = tokenizer.getChar()
	assert.Equal(t, '{', c)
	checkLocation(t, tokenizer.charLocation, line, column)
	checkLocation(t, tokenizer.location, line, column+1)
}

func dataFilePath(elem ...string) string {
	elem = append(elem, "")
	copy(elem[1:], elem)
	elem[0] = "resources"
	return filepath.Join(elem...)
}

func TestTokenizerGetChar(t *testing.T) {
	var source = "{foo: bar}"
	var reader io.Reader = strings.NewReader(source)
	var err error

	checkTokenizerGetChar(t, &reader, source)
	f, err := os.Open(dataFilePath("forms.cfg"))
	assert.Nil(t, err)
	defer closeFile(f)
	reader = bufio.NewReader(f)
	content, _ := ioutil.ReadAll(reader)
	expected := string(content)
	_, err = f.Seek(0, 0)
	assert.Nil(t, err)
	reader = bufio.NewReader(f)
	checkTokenizerGetChar(t, &reader, expected)
}

func checkToken(t *testing.T, expected *Token, actual *Token) {
	assert.Equal(t, expected.kind, actual.kind)
	assert.Equal(t, expected.text, actual.text)
	assert.Equal(t, expected.value, actual.value)
}

var EOFToken = Token{kind: EOF, text: "", value: nil}

func TestTokenizerGetToken(t *testing.T) {
	type tokenCase struct {
		ident  string
		source string
		tokens []Token
	}

	var W = makeWordToken

	tokenCases := []tokenCase{
		{
			ident:  "C16",
			source: "-.5",
			tokens: []Token{
				{kind: Float, text: "-.5", value: -0.5},
			},
		},
		{
			ident:  "C15",
			source: "a + \\\nb + \\\nc",
			tokens: []Token{
				W("a", 1, 1),
				{Plus, "+", nil, Location{1, 3}, Location{1, 3}},
				W("b", 2, 1),
				{Plus, "+", nil, Location{1, 3}, Location{1, 3}},
				W("c", 3, 1),
			},
		},
		{
			ident:  "C14",
			source: "1234_56_78",
			tokens: []Token{
				{kind: Integer, text: "1234_56_78", value: int64(12345678)},
			},
		},
		{
			ident:  "C13",
			source: "0o123_456",
			tokens: []Token{
				{kind: Integer, text: "0o123_456", value: int64(0xa72e)},
			},
		},
		{
			ident:  "C12",
			source: "0x1234_5678",
			tokens: []Token{
				{kind: Integer, text: "0x1234_5678", value: int64(0x12345678)},
			},
		},
		{
			ident:  "C11",
			source: "0b0001_0110_0111",
			tokens: []Token{
				{kind: Integer, text: "0b0001_0110_0111", value: int64(0x167)},
			},
		},
		{
			ident:  "C10",
			source: "0b000101100111",
			tokens: []Token{
				{kind: Integer, text: "0b000101100111", value: int64(0x167)},
			},
		},
		{
			ident:  "C09",
			source: "true false null and or not is in",
			tokens: []Token{
				{kind: True, text: "true", value: true},
				{kind: False, text: "false", value: false},
				{kind: None, text: "null", value: NullValue},
				{kind: And, text: "and", value: nil},
				{kind: Or, text: "or", value: nil},
				{kind: Not, text: "not", value: nil},
				{kind: Is, text: "is", value: nil},
				{kind: In, text: "in", value: nil},
			},
		},
		{
			ident:  "C08",
			source: "< > <= >= != <> << >> . * ** / //",
			tokens: []Token{
				{kind: LessThan, text: "<", value: nil},
				{kind: GreaterThan, text: ">", value: nil},
				{kind: LessThanOrEqual, text: "<=", value: nil},
				{kind: GreaterThanOrEqual, text: ">=", value: nil},
				{kind: Unequal, text: "!=", value: nil},
				{kind: AltUnequal, text: "<>", value: nil},
				{kind: LeftShift, text: "<<", value: nil},
				{kind: RightShift, text: ">>", value: nil},
				{kind: Dot, text: ".", value: nil},
				{kind: Star, text: "*", value: nil},
				{kind: Power, text: "**", value: nil},
				{kind: Slash, text: "/", value: nil},
				{kind: SlashSlash, text: "//", value: nil},
			},
		},
		{
			ident:  "C07",
			source: "{}[](),:@+-$&^!|%~",
			tokens: []Token{
				{kind: LeftCurly, text: "{", value: nil},
				{kind: RightCurly, text: "}", value: nil},
				{kind: LeftBracket, text: "[", value: nil},
				{kind: RightBracket, text: "]", value: nil},
				{kind: LeftParenthesis, text: "(", value: nil},
				{kind: RightParenthesis, text: ")", value: nil},
				{kind: Comma, text: ",", value: nil},
				{kind: Colon, text: ":", value: nil},
				{kind: At, text: "@", value: nil},
				{kind: Plus, text: "+", value: nil},
				{kind: Minus, text: "-", value: nil},
				{kind: Dollar, text: "$", value: nil},
				{kind: BitwiseAnd, text: "&", value: nil},
				{kind: BitwiseXor, text: "^", value: nil},
				{kind: Not, text: "!", value: nil},
				{kind: BitwiseOr, text: "|", value: nil},
				{kind: Modulo, text: "%", value: nil},
				{kind: BitwiseComplement, text: "~", value: nil},
			},
		},
		{
			ident:  "C06",
			source: "\n",
			tokens: []Token{
				{kind: Newline, text: "\n", value: nil},
			},
		},
		{
			ident:  "C05",
			source: ".7j",
			tokens: []Token{
				{kind: Complex, text: ".7j", value: complex(0, 0.7)},
			},
		},
		{
			ident:  "C04",
			source: ".5",
			tokens: []Token{
				{kind: Float, text: ".5", value: 0.5},
			},
		},
		{
			ident:  "C03",
			source: "'''baz'''",
			tokens: []Token{
				{kind: String, text: "'''baz'''", value: "baz"},
			},
		},
		{
			ident:  "C02",
			source: "'bar'",
			tokens: []Token{
				{kind: String, text: "'bar'", value: "bar"},
			},
		},
		{
			ident:  "C01",
			source: "foo",
			tokens: []Token{
				{kind: Word, text: "foo", value: "foo"},
			},
		},
	}
	for _, c := range tokenCases {
		var sr io.Reader = strings.NewReader(c.source)
		var tokenizer = newTokenizer(&sr)
		var token Token
		var err error

		for i, expected := range c.tokens {
			token, err = tokenizer.getToken()
			assert.Nil(t, err)
			checkToken(t, &expected, &token)
			if i >= 1000 {
				assert.Fail(t, "Too many tokens")
			}
		}
		token, err = tokenizer.getToken()
		checkToken(t, &EOFToken, &token)
	}
}

func TestTokenizerEmpties(t *testing.T) {
	type testCase struct {
		source string
		line   int
		column int
	}

	cases := []testCase{
		{
			source: "''",
			line:   1,
			column: 2,
		},
		{
			source: "\"\"",
			line:   1,
			column: 2,
		},
		{
			source: "''''''",
			line:   1,
			column: 6,
		},
		{
			source: "\"\"\"\"\"\"",
			line:   1,
			column: 6,
		},
	}

	for _, c := range cases {
		var sr io.Reader = strings.NewReader(c.source)
		var tokenizer = newTokenizer(&sr)

		token, err := tokenizer.getToken()
		assert.Nil(t, err)
		assert.NotNil(t, token)
		assert.Equal(t, String, token.kind)
		assert.Equal(t, c.source, token.text)
		assert.Equal(t, "", token.value)
		checkLocation(t, token.end, c.line, c.column)
	}
}

func checkError(t *testing.T, err error, message string, line int, col int) {
	assert.NotNil(t, err)
	if rerr, ok := err.(*RecognizerError); !ok {
		assert.Fail(t, "Unexpected error type returned: %T", err)
	} else {
		assert.Contains(t, rerr.message, message)
		if rerr.Location != nil && line > 0 && col >= 0 {
			checkLocation(t, *rerr.Location, line, col)
		}
	}
}

func TestTokenizerBadTokens(t *testing.T) {
	type badTokenCase struct {
		ident   string
		source  string
		message string
		line    int
		column  int
	}

	badTokenCases := []badTokenCase{
		{
			ident:   "C16",
			source:  " \\ ",
			message: "unexpected character: \\",
			line:    1,
			column:  3,
		},
		{
			ident:   "C15",
			source:  " 0._4e-8",
			message: "invalid '_' in number: 0._",
			line:    1,
			column:  4,
		},
		{
			ident:   "C14",
			source:  " 0.4_e-8",
			message: "invalid '_' at end of number: 0.4_",
			line:    1,
			column:  6,
		},
		{
			ident:   "C13",
			source:  " 0.4e-8_",
			message: "invalid '_' at end of number: 0.4e-8_",
			line:    1,
			column:  9,
		},
		{
			ident:   "C12",
			source:  "1_2__3",
			message: "invalid '_' in number: 1_2__",
			line:    1,
			column:  5,
		},
		{
			ident:   "C11",
			source:  "1__23",
			message: "invalid '_' in number: 1__",
			line:    1,
			column:  3,
		},
		{
			ident:   "C10",
			source:  "123_",
			message: "invalid '_' at end of number: 123_",
			line:    1,
			column:  5,
		},
		{
			ident:   "C09",
			source:  " 079",
			message: "badly-formed number: '079'",
			line:    1,
			column:  2,
		},
		{
			ident:   "C08",
			source:  " 0.4e-8Z",
			message: "unexpected character in number: Z",
			line:    1,
			column:  8,
		},
		{
			ident:   "C07",
			source:  "10z",
			message: "unexpected character in number: z",
			line:    1,
			column:  3,
		},
		{
			ident:   "C06",
			source:  "0X89g",
			message: "unexpected character in number: g",
			line:    1,
			column:  5,
		},
		{
			ident:   "C05",
			source:  "0o89z",
			message: "unexpected character in number: 8",
			line:    1,
			column:  3,
		},
		{
			ident:   "C04",
			source:  " 089z",
			message: "unexpected character in number: z",
			line:    1,
			column:  5,
		},
		{
			ident:   "C03",
			source:  " 0.4e-8.3",
			message: "unexpected character in number: .",
			line:    1,
			column:  8,
		},
		{
			ident:   "C02",
			source:  " 0.4e-z",
			message: "unexpected character in number: z",
			line:    1,
			column:  7,
		},
		{
			ident:   "C01",
			source:  "0.5.7",
			message: "unexpected character in number: .",
			line:    1,
			column:  4,
		},
	}

	for _, c := range badTokenCases {
		var sr io.Reader = strings.NewReader(c.source)
		var tokenizer = newTokenizer(&sr)
		var err error

		_, err = tokenizer.getToken()
		checkError(t, err, c.message, c.line, c.column)
	}
}

func TestTokenizerEscapes(t *testing.T) {
	type escapeCase struct {
		source   string
		expected string
	}

	escapeCases := []escapeCase{
		{
			source:   "'\\a'",
			expected: "\a",
		},
		{
			source:   "'\\b'",
			expected: "\b",
		},
		{
			source:   "'\\f'",
			expected: "\f",
		},
		{
			source:   "'\\n'",
			expected: "\n",
		},
		{
			source:   "'\\r'",
			expected: "\r",
		},
		{
			source:   "'\\t'",
			expected: "\t",
		},
		{
			source:   "'\\v'",
			expected: "\v",
		},
		{
			source:   "'\\\\'",
			expected: "\\",
		},
		{
			source:   "'\\''",
			expected: "'",
		},
		{
			source:   "'\\\"'",
			expected: "\"",
		},
		{
			source:   "'\\xAB'",
			expected: "\u00AB",
		},
		{
			source:   "'\\u2803'",
			expected: "\u2803",
		},
		{
			source:   "'\\u28a0abc\\u28a0'",
			expected: "\u28a0abc\u28a0",
		},
		{
			source:   "'\\u28a0abc'",
			expected: "\u28a0abc",
		},
		{
			source:   "'\\ue000'",
			expected: "\ue000",
		},
		{
			source:   "'\\U0010ffff'",
			expected: "\U0010ffff",
		},
	}

	for _, c := range escapeCases {
		var sr io.Reader = strings.NewReader(c.source)
		var tokenizer = newTokenizer(&sr)
		var err error

		token, err := tokenizer.getToken()
		assert.Nil(t, err)
		assert.Equal(t, c.expected, token.value)
	}

	badCases := []string{
		"'\\z'",
		"'\\x'",
		"'\\xa'",
		"'\\xaz'",
		"'\\u'",
		"'\\u0'",
		"'\\u01'",
		"'\\u012'",
		"'\\u012z'",
		"'\\u012zA'",
		"'\\ud800'",
		"'\\udfff'",
		"'\\U00110000'",
	}

	for _, s := range badCases {
		var sr io.Reader = strings.NewReader(s)
		var tokenizer = newTokenizer(&sr)
		var err error

		_, err = tokenizer.getToken()
		assert.NotNil(t, err)
	}
}

func TestTokenizerLocations(t *testing.T) {
	pf, err := os.Open(dataFilePath("pos.forms.cfg.txt"))
	assert.Nil(t, err)
	defer closeFile(pf)

	type positionCase struct {
		startLine int
		startCol  int
		endLine   int
		endCol    int
	}

	var expected = make([]positionCase, 0)

	var scanner = bufio.NewScanner(pf)
	for scanner.Scan() {
		var line = scanner.Text()
		var parts = strings.Split(line, " ")
		var nums [4]int

		for i, p := range parts {
			nums[i], err = strconv.Atoi(p)
			assert.Nil(t, err)
		}
		expected = append(expected, positionCase{nums[0], nums[1], nums[2], nums[3]})
	}

	f, err := os.Open(dataFilePath("forms.cfg"))
	assert.Nil(t, err)

	defer closeFile(f)
	var reader io.Reader

	reader = bufio.NewReader(f)
	var tokenizer = newTokenizer(&reader)

	for _, c := range expected {
		token, err := tokenizer.getToken()

		assert.Nil(t, err)
		checkLocation(t, token.start, c.startLine, c.startCol)
		checkLocation(t, token.end, c.endLine, c.endCol)
	}
}

func makeToken(kind tokenKind, text string, value Any, sl int, sc int, el int, ec int) Token {
	var start = Location{sl, sc}
	var end = Location{el, ec}

	return Token{kind, text, value, start, end}
}

func makeWordToken(text string, sl int, sc int) Token {
	ec := sc + len(text) - 1
	return makeToken(Word, text, text, sl, sc, sl, ec)
}

func makeNumberToken(text string, sl int, sc int) Token {
	ec := sc + len(text) - 1
	n, err := strconv.ParseInt(text, 10, 64)
	if err != nil {
		panic(fmt.Sprintf("failed to convert to integer: %s", text))
	}
	return makeToken(Integer, text, n, sl, sc, sl, ec)
}

func TestParserMisc(t *testing.T) {
	s := "'foo'   \"bar\"  '''baz'''"
	parser, err := makeParser(s)
	assert.Nil(t, err)
	var token Token
	var value Any

	var T = makeToken
	var W = makeWordToken
	var N = makeNumberToken

	token, err = parser.strings()
	assert.Nil(t, err)
	assert.Equal(t, "foobarbaz", token.value)
	checkLocation(t, token.start, 1, 1)
	checkLocation(t, token.end, 1, 24)

	type valueCase struct {
		source string
		value  Any
	}

	valueCases := []valueCase{
		{s, T(String, "foobarbaz", "foobarbaz", 1, 1, 1, 24)},
		{"foo", T(Word, "foo", "foo", 1, 1, 1, 3)},
		{"'foo'", T(String, "'foo'", "foo", 1, 1, 1, 5)},
		{"4", N("4", 1, 1)},
		{"-4", N("-4", 1, 1)},
		{"-1.0", T(Float, "-1.0", -1.0, 1, 1, 1, 4)},
		{"-1.0e-2", T(Float, "-1.0e-2", -0.01, 1, 1, 1, 7)},
		{"7j", T(Complex, "7j", complex(0, 7), 1, 1, 1, 2)},
		{"true", T(True, "true", true, 1, 1, 1, 4)},
		{"false", T(False, "false", false, 1, 1, 1, 5)},
		{"null", T(None, "null", NullValue, 1, 1, 1, 4)},
	}

	for _, c := range valueCases {
		parser, err = makeParser(c.source)
		assert.Nil(t, err)
		value, err = parser.value()
		assert.Nil(t, err)
		assert.Equal(t, c.value, value)
	}

	exprCases := []valueCase{
		{"!a", UnaryNode{op: Not, operand: W("a", 1, 2)}},
		{"a + b", BinaryNode{op: Plus, left: W("a", 1, 1), right: W("b", 1, 5)}},
		{"a * b", BinaryNode{op: Star, left: W("a", 1, 1), right: W("b", 1, 5)}},
		{"a ** b", BinaryNode{op: Power, left: W("a", 1, 1), right: W("b", 1, 6)}},
		{"a && b && c", BinaryNode{op: And, left: BinaryNode{op: And, left: W("a", 1, 1), right: W("b", 1, 6)}, right: W("c", 1, 11)}},
		{"a <= b", BinaryNode{op: LessThanOrEqual, left: W("a", 1, 1), right: W("b", 1, 6)}},
		{"a && (b || c)", BinaryNode{op: And, left: W("a", 1, 1), right: BinaryNode{op: Or, left: W("b", 1, 7), right: W("c", 1, 12)}}},
	}

	for _, c := range exprCases {
		parser, err = makeParser(c.source)
		assert.Nil(t, err)
		assert.NotNil(t, parser)
		value, err = parser.Expr()
		assert.NotNil(t, value)
		assert.Nil(t, err)
		if c.value != nil {
			assert.Equal(t, c.value, value)
		}
	}
}

func checkBinaryNode(t *testing.T, expected BinaryNode, actual BinaryNode) {
	assert.Equal(t, expected.op, actual.op)
	checkAny(t, expected.left, actual.left)
	checkAny(t, expected.right, actual.right)
}

func checkUnaryNode(t *testing.T, expected UnaryNode, actual UnaryNode) {
	assert.Equal(t, expected.op, actual.op)
	checkAny(t, expected.operand, actual.operand)
}

func checkSliceNode(t *testing.T, expected SliceNode, actual SliceNode) {
	checkAny(t, expected.start, actual.start)
	checkAny(t, expected.stop, actual.stop)
	checkAny(t, expected.step, actual.step)
}

func checkAny(t *testing.T, expected Any, actual Any) {
	var etk Token
	var bn BinaryNode
	var un UnaryNode
	var sn SliceNode

	if expected == nil {
		if actual == nil {
			return
		}
		assert.Fail(t, "nil expected", "actual: %v", actual)
	} else {
		if actual == nil {
			assert.Fail(t, "non-nil expected", "expected: %v", expected)
		}
	}
	etk, ok := expected.(Token)
	if ok {
		var atk Token
		atk, ok := actual.(Token)
		assert.True(t, ok)
		checkToken(t, &etk, &atk)
		return
	}
	bn, ok = expected.(BinaryNode)
	if ok {
		var abn BinaryNode

		abn, ok := actual.(BinaryNode)
		assert.True(t, ok)
		checkBinaryNode(t, bn, abn)
		return
	}
	un, ok = expected.(UnaryNode)
	if ok {
		var aun UnaryNode

		aun, ok := actual.(UnaryNode)
		assert.True(t, ok)
		checkUnaryNode(t, un, aun)
		return
	}
	sn, ok = expected.(SliceNode)
	if ok {
		var asn SliceNode

		asn, ok := actual.(SliceNode)
		assert.True(t, ok)
		checkSliceNode(t, sn, asn)
		return
	}
	assert.Fail(t, "Unhandled type", "%T", expected)
}

func exprNode(t *testing.T, s string) Any {
	parser, err := makeParser(s)
	assert.Nil(t, err)
	node, err := parser.Expr()
	assert.Nil(t, err)
	assert.NotNil(t, node)
	return node
}

func failExprNode(t *testing.T, s string) (Any, error) {
	parser, err := makeParser(s)
	assert.Nil(t, err)
	node, err := parser.Expr()
	return node, err
}

func TestParserSlices(t *testing.T) {
	var W = makeWordToken

	node := exprNode(t, "foo[start:stop:step]")
	expected := BinaryNode{Colon, W("foo", 1, 1),
		SliceNode{W("start", 1, 5),
			W("stop", 1, 11), W("step", 1, 16)}}
	checkAny(t, expected, node)
	node = exprNode(t, "foo[start:stop]")
	expected = BinaryNode{Colon, W("foo", 1, 1),
		SliceNode{W("start", 1, 5),
			W("stop", 1, 11), nil}}
	checkAny(t, expected, node)
	node = exprNode(t, "foo[start:stop:]")
	checkAny(t, expected, node)
	node = exprNode(t, "foo[start:]")
	expected = BinaryNode{Colon, W("foo", 1, 1),
		SliceNode{W("start", 1, 5),
			nil, nil}}
	checkAny(t, expected, node)
	node = exprNode(t, "foo[start::]")
	checkAny(t, expected, node)
	node = exprNode(t, "foo[:stop]")
	expected = BinaryNode{Colon, W("foo", 1, 1),
		SliceNode{nil, W("stop", 1, 6),
			nil}}
	checkAny(t, expected, node)
	node = exprNode(t, "foo[::step]")
	expected = BinaryNode{Colon, W("foo", 1, 1),
		SliceNode{nil, nil, W("step", 1, 7)}}
	checkAny(t, expected, node)
	node = exprNode(t, "foo[::]")
	expected = BinaryNode{Colon, W("foo", 1, 1),
		SliceNode{nil, nil, nil}}
	checkAny(t, expected, node)
	node = exprNode(t, "foo[start::step]")
	expected = BinaryNode{Colon, W("foo", 1, 1),
		SliceNode{W("start", 1, 5), nil, W("step", 1, 12)}}
	checkAny(t, expected, node)

	// non-slice case

	node = exprNode(t, "foo[start]")
	expected = BinaryNode{LeftBracket, W("foo", 1, 1),
		W("start", 1, 5)}
	checkAny(t, expected, node)

	// failure cases

	node, err := failExprNode(t, "foo[start::step:]")
	assert.NotNil(t, err)
	node, err = failExprNode(t, "foo[a, b:c:d]")
	assert.NotNil(t, err)
	node, err = failExprNode(t, "foo[a:b, c:d]")
	assert.NotNil(t, err)
	node, err = failExprNode(t, "foo[a:b:c,d, e]")
	assert.NotNil(t, err)
}

func TestParserFromData(t *testing.T) {
	testData, err := loadData(dataFilePath("testdata.txt"))
	var node Any

	if err == nil {
		for k, s := range testData {
			var fmsg = fmt.Sprintf("Failed for %v", k)
			if strings.Compare(k, "D01") < 0 {
				node, err = Parse(s)
				assert.Nil(t, err, fmsg)
				assert.NotNil(t, node, fmsg)
			} else {
				node, err = Parse(s)
				assert.NotNil(t, err, fmsg)
			}
		}
	}
}

func makeMap(kvpList []keyValue) Mapping {
	var result = make(Mapping)

	for _, kvp := range kvpList {
		result[kvp.key.value.(string)] = kvp.value
	}
	return result
}

func TestParserJson(t *testing.T) {
	f, err := os.Open(dataFilePath("forms.conf"))
	assert.Nil(t, err)

	defer closeFile(f)
	var reader io.Reader

	reader = bufio.NewReader(f)
	parser, err := NewParser(&reader)
	assert.Nil(t, err)
	kvpList, err := parser.mapping()
	assert.Nil(t, err)
	assert.NotNil(t, kvpList)
	node := makeMap(kvpList.([]keyValue))
	assert.Contains(t, node, "refs")
	assert.Contains(t, node, "fieldsets")
	assert.Contains(t, node, "forms")
	assert.Contains(t, node, "modals")
	assert.Contains(t, node, "pages")
}

func TestParserForms(t *testing.T) {
	f, err := os.Open(dataFilePath("forms.cfg"))
	assert.Nil(t, err)

	defer closeFile(f)
	var reader io.Reader

	reader = bufio.NewReader(f)
	parser, err := NewParser(&reader)
	assert.Nil(t, err)
	kvpList, err := parser.MappingBody()
	assert.Nil(t, err)
	assert.NotNil(t, kvpList)
	node := makeMap(kvpList.([]keyValue))
	assert.Contains(t, node, "refs")
	assert.Contains(t, node, "fieldsets")
	assert.Contains(t, node, "forms")
	assert.Contains(t, node, "modals")
	assert.Contains(t, node, "pages")
}

func mustGetValue(t *testing.T, cfg *Config, key string) Any {
	result, err := cfg.Get(key)
	assert.Nil(t, err)
	return result
}

func mustGetMappingValue(t *testing.T, cfg *Config, key string) Mapping {
	var result Mapping

	v := mustGetValue(t, cfg, key)

	if mv, ok := v.(MapWrapper); !ok {
		assert.Fail(t, fmt.Sprintf("Expected mapping but got %v", v))
	} else {
		m, err := mv.AsDict()
		assert.Nil(t, err)
		result = m
	}
	return result
}

func mustGetSequenceValue(t *testing.T, cfg *Config, key string) Sequence {
	var result Sequence

	v := mustGetValue(t, cfg, key)

	if sv, ok := v.(SeqWrapper); !ok {
		assert.Fail(t, fmt.Sprintf("Expected sequence but got %v", v))
	} else {
		lv, err := sv.AsList()
		assert.Nil(t, err)
		result = lv
	}
	return result
}

func mustGetConfigValue(t *testing.T, cfg *Config, key string) *Config {
	var result *Config

	v := mustGetValue(t, cfg, key)

	if c, ok := v.(*Config); !ok {
		assert.Fail(t, fmt.Sprintf("Expected config but got %v", v))
	} else {
		result = c
	}
	return result
}

func TestConfigDuplicates(t *testing.T) {
	var p = dataFilePath("derived", "dupes.cfg")

	V := mustGetValue

	config, err := FromFile(p)
	checkError(t, err, "Duplicate key foo at (4, 1) (previously at (1, 1))", 0, 0)

	// now try after allowing duplicates
	config.NoDuplicates = false
	err = config.LoadFile(p)
	assert.Nil(t, err)
	s := V(t, config, "foo")
	assert.Equal(t, "not again!", s)
}

func TestConfigIdentifiers(t *testing.T) {
	type pathCase struct {
		source string
		result bool
	}
	cases := []pathCase{
		{"foo", true},
		{"\u0935\u092e\u0938", true},
		{"\u73b0\u4ee3\u6c49\u8bed\u5e38\u7528\u5b57\u8868", true},
		{"foo ", false},
		{"foo[", false},
		{"foo [", false},
		{"foo.", false},
		{"foo .", false},
		{"\u0935\u092e\u0938.", false},
		{"\u73b0\u4ee3\u6c49\u8bed\u5e38\u7528\u5b57\u8868.", false},
		{"9", false},
		{"9foo", false},
		{"hyphenated-key", false},
	}

	for _, c := range cases {
		r := isIdentifier(c.source)
		assert.Equal(t, c.result, r)
	}
}

func TestConfigBadPaths(t *testing.T) {
	type pathCase struct {
		source  string
		message string
		line    int
		col     int
	}
	badPaths := []pathCase{
		{"foo[1, 2]", "invalid index:", 1, 9},
		{"foo[1].bar baz", "Invalid Path: foo[1].bar baz", 1, 12},
		{"foo.123", "Invalid Path: foo.123", 1, 4},
		{"foo.", "expected Word but got EOF", 1, 5},
		{"foo[]", "invalid index: expected 1 expression, found 0", 1, 5},
		{"4", "Invalid Path: 4", 0, 0},
		{"foo[1a]", "unexpected character in number: a", 1, 6},
	}

	for _, c := range badPaths {
		_, err := parsePath(c.source)
		checkError(t, err, c.message, c.line, c.col)
	}
}

func collectPathItems(ch <-chan Any) []Any {
	var result = make([]Any, 0)

	for item := range ch {
		result = append(result, item)
	}
	return result
}

func TestConfigPathIteration(t *testing.T) {
	type pathIterCase struct {
		source string
		result []Any
	}

	var W = makeWordToken
	var N = makeNumberToken

	cases := []pathIterCase{
		{"foo[bar].baz.bozz[3].fizz", []Any{
			W("foo", 1, 1),
			pathValue{LeftBracket, "bar"},
			pathValue{Dot, "baz"},
			pathValue{Dot, "bozz"},
			pathValue{LeftBracket, int64(3)},
			pathValue{Dot, "fizz"},
		}},
		{"foo[1:2]", []Any{
			W("foo", 1, 1),
			pathValue{Colon, SliceNode{N("1", 1, 5), N("2", 1, 7), nil}},
		}},
	}

	for _, c := range cases {
		node, err := parsePath(c.source)
		assert.Nil(t, err)
		assert.NotNil(t, node)
		parts := collectPathItems(pathIterator(node))
		assert.Equal(t, c.result, parts)
	}
}

func TestConfigContext(t *testing.T) {
	var p = dataFilePath("derived", "context.cfg")

	V := mustGetValue

	config := NewConfig()
	context := Mapping{
		"bozz": "bozz-bozz",
	}
	config.Context = &context
	err := config.LoadFile(p)
	assert.Nil(t, err)
	v := V(t, config, "baz")
	assert.Equal(t, "bozz-bozz", v)
	v, err = config.Get("bad")
	checkError(t, err, "Unknown variable 'not_there'", 0, 0)
}

func TestConfigMainConfig(t *testing.T) {
	p := dataFilePath("derived", "main.cfg")
	b := dataFilePath("base")

	V := mustGetValue
	MV := mustGetMappingValue
	SV := mustGetSequenceValue

	config := NewConfig()
	config.IncludePath = append(config.IncludePath, b)
	err := config.LoadFile(p)
	assert.Nil(t, err)
	v := V(t, config, "logging")
	assert.NotNil(t, v)
	logConf := mustGetConfigValue(t, config, "logging")
	v, err = logConf.Get("handlers.file/filename")
	checkError(t, err, "Invalid Path: handlers.file/filename", 1, 14)
	v, err = logConf.Get("\"handlers.file/filename")
	checkError(t, err, "Invalid Path: \"handlers.file/filename", 1, 1)
	v, err = logConf.GetWithDefault("foo", "bar")
	assert.Nil(t, err)
	assert.Equal(t, "bar", v)
	v, err = logConf.GetWithDefault("foo.bar", "baz")
	assert.Nil(t, err)
	assert.Equal(t, "baz", v)
	v, err = logConf.GetWithDefault("handlers.debug.levl", "bozz")
	assert.Nil(t, err)
	assert.Equal(t, "bozz", v)
	assert.Equal(t, "run/server.log", V(t, logConf, "handlers.file.filename"))
	assert.Equal(t, "run/server-debug.log", V(t, logConf, "handlers.debug.filename"))
	v = SV(t, logConf, "root.handlers")
	assert.Equal(t, Sequence{"file", "error", "debug"}, v)
	v = SV(t, logConf, "root.handlers[:2]")
	assert.Equal(t, Sequence{"file", "error"}, v)
	v = SV(t, logConf, "root.handlers[::2]")
	assert.Equal(t, Sequence{"file", "debug"}, v)

	testConf := mustGetConfigValue(t, config, "test")
	assert.Equal(t, 1.0e-7, V(t, testConf, "float"))
	assert.Equal(t, 0.3, V(t, testConf, "float2"))
	assert.Equal(t, 3.0, V(t, testConf, "float3"))
	v, err = testConf.Get("float4")
	assert.NotNil(t, err)
	assert.Equal(t, int64(2), V(t, testConf, "list[1]"))
	assert.Equal(t, "b", V(t, testConf, "dict.a"))
	v = V(t, testConf, "date")
	expTime, err := time.Parse(time.RFC3339, "2019-03-28T00:00:00Z")
	assert.Equal(t, expTime, v)
	v = V(t, testConf, "date_time")
	expTime, err = time.Parse(time.RFC3339, "2019-03-28T23:27:04.314159+05:30")
	assert.Equal(t, expTime, v)
	v = V(t, testConf, "neg_date_time")
	expTime, err = time.Parse(time.RFC3339, "2019-03-28T23:27:04.314159-05:30")
	assert.Equal(t, expTime, v)
	v = V(t, testConf, "alt_date_time")
	expTime, err = time.Parse(time.RFC3339, "2019-03-28T23:27:04.271828Z")
	assert.Equal(t, expTime, v)
	v = V(t, testConf, "no_ms_time")
	expTime, err = time.Parse(time.RFC3339, "2019-03-28T23:27:04Z")
	assert.Equal(t, expTime, v)
	assert.Equal(t, 3.3, V(t, testConf, "computed"))
	assert.Equal(t, 2.7, V(t, testConf, "computed2"))
	assert.InDelta(t, 0.9, V(t, testConf, "computed3"), 1e-7)
	assert.Equal(t, 10.0, V(t, testConf, "computed4"))
	_ = mustGetConfigValue(t, config, "base")
	v = SV(t, config, "combined_list")
	explist := Sequence{"derived_foo", "derived_bar", "derived_baz",
		"test_foo", "test_bar", "test_baz",
		"base_foo", "base_bar", "base_baz"}
	assert.Equal(t, explist, v)
	v = MV(t, config, "combined_map_1")
	expmap := Mapping{
		"foo_key":         "base_foo",
		"bar_key":         "base_bar",
		"baz_key":         "base_baz",
		"base_foo_key":    "base_foo",
		"base_bar_key":    "base_bar",
		"base_baz_key":    "base_baz",
		"derived_foo_key": "derived_foo",
		"derived_bar_key": "derived_bar",
		"derived_baz_key": "derived_baz",
		"test_foo_key":    "test_foo",
		"test_bar_key":    "test_bar",
		"test_baz_key":    "test_baz",
	}
	assert.Equal(t, expmap, v)
	v = MV(t, config, "combined_map_2")
	expmap = Mapping{
		"derived_foo_key": "derived_foo",
		"derived_bar_key": "derived_bar",
		"derived_baz_key": "derived_baz",
	}
	assert.Equal(t, expmap, v)
	n1 := V(t, config, "number_1")
	n2 := V(t, config, "number_2")
	n3 := V(t, config, "number_3")
	n4 := V(t, config, "number_4")
	assert.Equal(t, n1.(int64)&n2.(int64), n3)
	assert.Equal(t, n1.(int64)^n2.(int64), n4)

	type pathCase struct {
		path    string
		message string
		line    int
		col     int
	}
	cases := []pathCase{
		{"logging[4]", "string required, but found int64(4)", 0, 0},
		{"logging[:4]", "slices can only operate on lists", 0, 0},
	}
	for _, c := range cases {
		v, err = config.Get(c.path)
		checkError(t, err, c.message, c.line, c.col)
	}
}

func TestConfigExampleConfig(t *testing.T) {
	p := dataFilePath("derived", "example.cfg")
	b := dataFilePath("base")

	V := mustGetValue
	SV := mustGetSequenceValue

	config := NewConfig()
	config.IncludePath = append(config.IncludePath, b)
	err := config.LoadFile(p)
	assert.Nil(t, err)

	// strings
	v1 := V(t, config, "snowman_escaped")
	v2 := V(t, config, "snowman_unescaped")
	assert.Equal(t, v1, v2)
	assert.Equal(t, "\u2603", v1)
	v1 = V(t, config, "face_with_tears_of_joy")
	v2 = V(t, config, "unescaped_face_with_tears_of_joy")
	assert.Equal(t, "\U0001F602", v1)
	assert.Equal(t, v1, v2)
	v1 = SV(t, config, "strings")
	if runtime.GOOS == "windows" {
		v2 = Sequence{
			"Oscar Fingal O'Flahertie Wills Wilde",
			"size: 5\"",
			"Triple quoted form\r\ncan span\r\n'multiple' lines",
			"with \"either\"\r\nkind of 'quote' embedded within",
		}
	} else {
		v2 = Sequence{
			"Oscar Fingal O'Flahertie Wills Wilde",
			"size: 5\"",
			"Triple quoted form\ncan span\n'multiple' lines",
			"with \"either\"\nkind of 'quote' embedded within",
		}
	}
	assert.Equal(t, v2, v1)

	// special strings
	// v := V(t, config, "special_value_1") No access to object values in Go
	v := V(t, config, "special_value_2")
	assert.Equal(t, os.Getenv("HOME"), v)
	v = V(t, config, "special_value_3")
	expTime, err := time.Parse(datetimeLayout, "2019-03-28T23:27:04.314159+05:30:43")
	assert.Equal(t, expTime, v)
	v = V(t, config, "special_value_4")
	assert.Equal(t, "bar", v)

	// integers
	assert.Equal(t, int64(123), V(t, config, "decimal_integer"))
	assert.Equal(t, int64(0x123), V(t, config, "hexadecimal_integer"))
	assert.Equal(t, int64(83), V(t, config, "octal_integer"))
	assert.Equal(t, int64(291), V(t, config, "binary_integer"))

	// floats
	assert.Equal(t, 123.456, V(t, config, "common_or_garden"))
	assert.Equal(t, .123, V(t, config, "leading_zero_not_needed"))
	assert.Equal(t, 123.0, V(t, config, "trailing_zero_not_needed"))
	assert.Equal(t, 1.0e6, V(t, config, "scientific_large"))
	assert.Equal(t, 1.0e-7, V(t, config, "scientific_small"))
	assert.Equal(t, 3.14159, V(t, config, "expression_1"))

	// complex
	assert.Equal(t, complex(3.0, 2.0), V(t, config, "expression_2"))
	assert.Equal(t, complex(1.0, 3.0), V(t, config, "list_value[4]"))

	// boolean
	assert.True(t, V(t, config, "boolean_value").(bool))
	assert.False(t, V(t, config, "opposite_boolean_value").(bool))
	assert.False(t, V(t, config, "computed_boolean_2").(bool))
	assert.True(t, V(t, config, "computed_boolean_1").(bool))

	// sequence
	seq := SV(t, config, "incl_list")
	assert.Equal(t, Sequence{"a", "b", "c"}, seq)

	// mapping
	im := mustGetConfigValue(t, config, "incl_mapping")
	mapping, err := im.AsDict()
	assert.Nil(t, err)
	assert.Equal(t, Mapping{"foo": "bar", "bar": "baz"}, mapping)

	// mapping body
	im = mustGetConfigValue(t, config, "incl_mapping_body")
	mapping, err = im.AsDict()
	assert.Nil(t, err)
	assert.Equal(t, Mapping{"baz": "bozz", "fizz": "buzz"}, mapping)
}

func fromPath(t *testing.T, path string) (*Config, error) {
	result, err := FromFile(path)

	assert.Nil(t, err)
	assert.NotNil(t, result)
	p, perr := filepath.Abs(path)
	assert.Nil(t, perr)
	assert.Equal(t, p, result.Path)
	return result, err
}

func TestConfigExpressions(t *testing.T) {
	p := dataFilePath("derived", "test.cfg")

	V := mustGetValue
	MV := mustGetMappingValue
	SV := mustGetSequenceValue

	config, err := fromPath(t, p)
	mapping := MV(t, config, "dicts_added")
	assert.Equal(t, Mapping{"a": "b", "c": "d"}, mapping)
	mapping = MV(t, config, "nested_dicts_added")
	expmap := Mapping{
		"a": Mapping{"b": "c", "w": "x"},
		"d": Mapping{"e": "f", "y": "z"},
	}
	assert.Equal(t, expmap, mapping)
	seq := SV(t, config, "lists_added")
	assert.Equal(t, Sequence{"a", int64(1), "b", int64(2)}, seq)
	mapping = MV(t, config, "dicts_subtracted")
	assert.Equal(t, Mapping{"a": "b"}, mapping)
	mapping = MV(t, config, "nested_dicts_subtracted")
	assert.Equal(t, Mapping{}, mapping)
	mapping = MV(t, config, "dict_with_nested_stuff")
	expmap = Mapping{
		"a_list": Sequence{int64(1), int64(2), Mapping{"a": int64(3)}},
		"a_map":  Mapping{"k1": Sequence{"b", "c", Mapping{"d": "e"}}},
	}
	assert.Equal(t, expmap, mapping)
	seq = SV(t, config, "dict_with_nested_stuff.a_list[:2]")
	assert.Equal(t, Sequence{int64(1), int64(2)}, seq)
	v := V(t, config, "unary")
	assert.Equal(t, int64(-4), v)
	v = V(t, config, "abcdefghijkl")
	assert.Equal(t, "mno", v)
	v = V(t, config, "power")
	assert.Equal(t, int64(8), v)
	v = V(t, config, "computed5")
	assert.Equal(t, 2.5, v)
	v = V(t, config, "computed6")
	assert.Equal(t, int64(2), v)
	v = V(t, config, "c3")
	assert.Equal(t, complex(3, 1), v)
	v = V(t, config, "c4")
	assert.Equal(t, complex(5, 5), v)
	v, err = config.Get("bad_include")
	assert.NotNil(t, err)
	checkError(t, err, "@ operand must be a string, but is: 4", 0, 0)
	v, err = config.Get("computed7")
	checkError(t, err, "Not found in configuration: float4", 0, 0)
	v = V(t, config, "computed8")
	assert.Equal(t, int64(2), v)
	v = V(t, config, "computed9")
	assert.Equal(t, int64(160), v)
	v = V(t, config, "computed10")
	assert.Equal(t, int64(62), v)
	v = V(t, config, "dict.a")
	assert.Equal(t, "b", v)
	//  fetch again - should have same value
	v = V(t, config, "dict.a")
	assert.Equal(t, "b", v)
	v = V(t, config, "f.g")
	assert.Equal(t, "h", v)
	v = V(t, config, "interp")
	assert.Equal(t, "A-4 a test_foo true 10 1e-07 1 b [a, c, e, g]Z", v)
	v = V(t, config, "interp2")
	assert.Equal(t, "{a: b}", v)

	type pathCase struct {
		path    string
		message string
		line    int
		col     int
	}
	cases := []pathCase{
		{"dict[4]", "string required, but found 4", 0, 0},
		{"list['foo']", "integer required, but found 'foo'", 0, 0},
		{"dict[:4]", "slices can only operate on lists", 0, 0},
		{"bad_interp", "Unable to convert string ", 0, 0},
	}
	for _, c := range cases {
		v, err = config.Get(c.path)
		checkError(t, err, c.message, c.line, c.col)
	}
}

func TestConfigForms(t *testing.T) {
	p := dataFilePath("derived", "forms.cfg")
	b := dataFilePath("base")

	//V := mustGetValue
	MV := mustGetMappingValue
	//SV := mustGetSequenceValue

	config := NewConfig()
	config.IncludePath = append(config.IncludePath, b)
	err := config.LoadFile(p)
	assert.Nil(t, err)
	d := MV(t, config, "modals.deletion.contents[0]")
	assert.Equal(t, "frm-deletion", d["id"])

	type testCase struct {
		path   string
		result Mapping
	}

	cases := []testCase{
		{"refs.delivery_address_field", Mapping{
			"kind":        "field",
			"type":        "textarea",
			"name":        "postal_address",
			"label":       "Postal address",
			"label_i18n":  "postal-address",
			"short_name":  "address",
			"placeholder": "We need this for delivering to you",
			"ph_i18n":     "your-postal-address",
			"message":     " ",
			"required":    true,
			"attrs":       Mapping{"minlength": int64(10)},
			"grpclass":    "col-md-6",
		}},
		{"refs.delivery_instructions_field", Mapping{
			"kind":        "field",
			"type":        "textarea",
			"name":        "delivery_instructions",
			"label":       "Delivery Instructions",
			"short_name":  "notes",
			"placeholder": "Any special delivery instructions?",
			"message":     " ",
			"label_i18n":  "delivery-instructions",
			"ph_i18n":     "any-special-delivery-instructions",
			"grpclass":    "col-md-6",
		}},
		{"refs.verify_field", Mapping{
			"kind":        "field",
			"type":        "input",
			"name":        "verification_code",
			"label":       "Verification code",
			"label_i18n":  "verification-code",
			"short_name":  "verification code",
			"placeholder": "Your verification code (NOT a backup code)",
			"ph_i18n":     "verification-not-backup-code",
			"attrs": Mapping{
				"minlength": int64(6),
				"maxlength": int64(6),
				"autofocus": true},
			"append": Mapping{
				"label":   "Verify",
				"type":    "submit",
				"classes": "btn-primary"},
			"message":  " ",
			"required": true,
		}},
		{"refs.signup_password_field", Mapping{
			"kind":        "field",
			"type":        "password",
			"label":       "Password",
			"label_i18n":  "password",
			"message":     " ",
			"name":        "password",
			"ph_i18n":     "password-wanted-on-site",
			"placeholder": "The password you want to use on this site",
			"required":    true,
			"toggle":      true,
		}},
		{"refs.signup_password_conf_field", Mapping{
			"kind":        "field",
			"type":        "password",
			"name":        "password_conf",
			"label":       "Password confirmation",
			"label_i18n":  "password-confirmation",
			"placeholder": "The same password, again, to guard against mistyping",
			"ph_i18n":     "same-password-again",
			"message":     " ",
			"toggle":      true,
			"required":    true,
		}},
		{"fieldsets.signup_ident[0].contents[0]", Mapping{
			"kind":        "field",
			"type":        "input",
			"name":        "display_name",
			"label":       "Your name",
			"label_i18n":  "your-name",
			"placeholder": "Your full name",
			"ph_i18n":     "your-full-name",
			"message":     " ",
			"data_source": "user.display_name",
			"required":    true,
			"attrs":       Mapping{"autofocus": true},
			"grpclass":    "col-md-6",
		}},
		{"fieldsets.signup_ident[0].contents[1]", Mapping{
			"kind":        "field",
			"type":        "input",
			"name":        "familiar_name",
			"label":       "Familiar name",
			"label_i18n":  "familiar-name",
			"placeholder": "If not just the first word in your full name",
			"ph_i18n":     "if-not-first-word",
			"data_source": "user.familiar_name",
			"message":     " ",
			"grpclass":    "col-md-6",
		}},
		{"fieldsets.signup_ident[1].contents[0]", Mapping{
			"kind":        "field",
			"type":        "email",
			"name":        "email",
			"label":       "Email address (used to sign in)",
			"label_i18n":  "email-address",
			"short_name":  "email address",
			"placeholder": "Your email address",
			"ph_i18n":     "your-email-address",
			"message":     " ",
			"required":    true,
			"data_source": "user.email",
			"grpclass":    "col-md-6",
		}},
		{"fieldsets.signup_ident[1].contents[1]", Mapping{
			"kind":        "field",
			"type":        "input",
			"name":        "mobile_phone",
			"label":       "Phone number",
			"label_i18n":  "phone-number",
			"short_name":  "phone number",
			"placeholder": "Your phone number",
			"ph_i18n":     "your-phone-number",
			"classes":     "numeric",
			"message":     " ",
			"prepend":     Mapping{"icon": "phone"},
			"attrs":       Mapping{"maxlength": int64(10)},
			"required":    true,
			"data_source": "customer.mobile_phone",
			"grpclass":    "col-md-6",
		}},
		/*
			{"", Mapping{

			}},
		*/
	}
	for _, c := range cases {
		m := MV(t, config, c.path)
		assert.Equal(t, c.result, m)
	}
}

func TestConfigPathAcrossIncludes(t *testing.T) {
	p := dataFilePath("derived", "main.cfg")
	b := dataFilePath("base")

	V := mustGetValue
	//MV := mustGetMappingValue
	//SV := mustGetSequenceValue

	config := NewConfig()
	config.IncludePath = append(config.IncludePath, b)
	err := config.LoadFile(p)
	assert.Nil(t, err)
	v := V(t, config, "logging.handlers.file.filename")
	assert.Equal(t, "run/server.log", v)
	v = V(t, config, "logging.handlers.debug.filename")
	assert.Equal(t, "run/server-debug.log", v)
	v = V(t, config, "logging.handlers.error.filename")
	assert.Equal(t, "run/server-errors.log", v)
	v = V(t, config, "redirects.freeotp.url")
	assert.Equal(t, "https://freeotp.github.io/", v)
}

func TestConfigSources(t *testing.T) {
	cases := []string{
		"foo[::2]",
		"foo[:]",
		"foo[:2]",
		"foo[2:]",
		"foo[::1]",
		"foo[::-1]",
	}
	for _, s := range cases {
		node, err := parsePath(s)
		assert.Nil(t, err)
		assert.NotNil(t, node)
		v := toSource(node)
		assert.Equal(t, s, v)
	}
}

func TestConfigBadConversions(t *testing.T) {
	cases := []string{
		"foo",
	}
	config := NewConfig()
	for _, s := range cases {
		config.StrictConversions = true
		v, err := config.convertString(s)
		checkError(t, err, fmt.Sprintf("Unable to convert string %v", s), 0, 0)
		config.StrictConversions = false
		v, err = config.convertString(s)
		assert.Nil(t, err)
		assert.Equal(t, s, v)
	}
}

func TestConfigCircularReferences(t *testing.T) {
	p := dataFilePath("derived", "test.cfg")

	config, err := fromPath(t, p)
	assert.Nil(t, err)
	_, err = config.Get("circ_list[1]")
	checkError(t, err, "Circular reference: circ_list[1] (42, 7)", 0, 0)
	_, err = config.Get("circ_map.a")
	checkError(t, err, "Circular reference: circ_map.a (49, 10), circ_map.b (47, 10), circ_map.c (48, 10)", 0, 0)
}

func TestConfigSlicesAndIndices(t *testing.T) {
	p := dataFilePath("derived", "test.cfg")
	theList := Sequence{"a", "b", "c", "d", "e", "f", "g"}

	V := mustGetValue
	//MV := mustGetMappingValue
	SV := mustGetSequenceValue

	config, err := fromPath(t, p)
	assert.Nil(t, err)
	assert.NotNil(t, config)

	// slices

	type testCase struct {
		key    string
		result Sequence
	}

	cases := []testCase{
		{"test_list[:]", theList},
		{"test_list[::]", theList},
		{"test_list[:20]", theList},
		{"test_list[-20:4]", Sequence{"a", "b", "c", "d"}},
		{"test_list[-20:20]", theList},
		{"test_list[2:]", Sequence{"c", "d", "e", "f", "g"}},
		{"test_list[-3:]", Sequence{"e", "f", "g"}},
		{"test_list[-2:2:-1]", Sequence{"f", "e", "d"}},
		{"test_list[::-1]", Sequence{"g", "f", "e", "d", "c", "b", "a"}},
		{"test_list[2:-2:2]", Sequence{"c", "e"}},
		{"test_list[::2]", Sequence{"a", "c", "e", "g"}},
		{"test_list[::3]", Sequence{"a", "d", "g"}},
		{"test_list[::2][::3]", Sequence{"a", "g"}},
	}
	for _, c := range cases {
		v := SV(t, config, c.key)
		assert.Equal(t, c.result, v)
	}

	// indices

	for i, v := range theList {
		k := fmt.Sprintf("test_list[%d]", i)
		cv := V(t, config, k)
		assert.Equal(t, v, cv)
	}

	// negative indices

	n := len(theList)
	for i := n; i > 0; i-- {
		k := fmt.Sprintf("test_list[-%d]", i)
		cv := V(t, config, k)
		assert.Equal(t, theList[n-i], cv)
	}

	// invalid indices
	badIndices := []int{n, n + 1, -(n + 1), -(n + 2)}
	for _, i := range badIndices {
		k := fmt.Sprintf("test_list[%d]", i)
		_, err := config.Get(k)
		checkError(t, err, "index out of range: ", 0, 0)
	}
}

func TestAbsoluteIncludePath(t *testing.T) {
	V := mustGetValue

	p, err := filepath.Abs(dataFilePath("derived", "test.cfg"))
	p = strings.Replace(p, "\\", "/", -1)
	source := strings.Replace("test: @'foo'", "foo", p, -1)
	var sr io.Reader = strings.NewReader(source)
	cfg := NewConfig()
	err = cfg.Load(&sr)
	assert.Nil(t, err)
	cv := V(t, cfg, "test.computed6")
	assert.Equal(t, int64(2), cv)
}

func TestNestedIncludePath(t *testing.T) {
	V := mustGetValue
	p := dataFilePath("base", "top.cfg")

	cfg, err := fromPath(t, p)
	assert.Nil(t, err)
	cfg.IncludePath = append(cfg.IncludePath, dataFilePath("derived"))
	cfg.IncludePath = append(cfg.IncludePath, dataFilePath("another"))
	cv := V(t, cfg, "level1.level2.final")
	assert.Equal(t, int64(42), cv)
}

func TestRecursiveConfiguration(t *testing.T) {
	p := dataFilePath("derived", "recurse.cfg")

	cfg, err := fromPath(t, p)
	assert.Nil(t, err)
	_, err = cfg.Get("recurse")
	checkError(t, err, "Configuration cannot include itself: recurse.cfg", 0, 0)
}
