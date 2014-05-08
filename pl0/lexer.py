#!/usr/bin/python

__doc__='''Simple lexer for PL/0 using generators'''

## # Tokens can have multiple definitions if needed
## symbols =  {
## 	'lparen' : ['('],
## 	'rparen' : [')'],
## 	'times'  : ['*'],
## 	'slash'  : ['/'],
## 	'plus'   : ['+'],
## 	'minus'  : ['-'],
## 	'eql'    : ['='],
## 	'neq'    : ['!='],
## 	'lss'    : ['<'],
## 	'leq'    : ['<='],
## 	'gtr'    : ['>'],
## 	'geq'    : ['>='],
## 	'callsym': ['call'],
## 	'beginsym'  : ['begin'],
## 	'semicolon' : [';'],
## 	'endsym'    : ['end'],
## 	'ifsym'     : ['if'],
## 	'whilesym'  : ['while'],
## 	'becomes'   : [':='],
## 	'thensym'   : ['then'],
## 	'dosym'     : ['do'],
## 	'constsym'  : ['const'],
## 	'comma'     : [','],
## 	'varsym'    : ['var'],
## 	'procsym'   : ['procedure'],
## 	'period'    : ['.'],
## 	'oddsym'    : ['odd'],
## 	'print'			: ['!', 'print'],
## }

# Tokens can have multiple definitions if needed
symbols =  {
	'lparen'        : ['('],
	'rparen'        : [')'],
	'times'         : ['*'],
	'slash'         : ['/'],
	'plus'          : ['+'],
	'minus'         : ['-'],
    'mod_sym'       : ['mod'],
	'eql'           : ['eq'],
	'neq_sym'       : ['neq'],
	'lss'           : ['<'],
	'leq'           : ['<='],
	'gtr'           : ['>'],
	'geq'           : ['>='],
    'and_sym'       : ['and'],
    'or_sym'        : ['or'],
    'not_sym'       : ['not'],
	'if_sym'        : ['if'],
    'else_sym'      : ['else'],
	'becomes'       : ['<-'],
	'comma'         : [','],
	'period'        : ['.'],
	'at_sym'        : ['@'],
	'hash_sym'      : ['#'],
    'int_sym'       : ['int'],
    'float_sym'     : ['float'],
    'bool_sym'      : ['bool'],
    'char_sym'      : ['char'],
    'vect_sym'      : ['vector'],
    'of_sym'        : ['of'],
    'quote_sym'     : ['\''],
    'incr_sym'      : ['incr'],
    'decr_sym'      : ['decr'],
    'ret_sym'       : ['return'],
    'repeat_sym'    : ['repeat'],
    'until_sym'     : ['until'],
    'for_sym'       : ['foreach'],
    'in_sym'        : ['in'],
    'dec_sym'       : ['dec'],
    'colon'         : [':'],
    'block_sym'     : ['->'],
    'is_sym'        : ['is']
}

def token(word):
	'''Return corresponding token for a given word'''
	for s in symbols :
		if word in symbols[s] :
			return s
	try : # If a terminal is not one of the standard tokens but can be converted to float, then it is a number, otherwise, an identifier
		float(word)
		return 'number'
	except ValueError, e :
		return 'ident'

def lexer(text) :
	'''Generator implementation of a lexer'''
	import re
	from string import split, strip, lower, join
	t=re.split('(\W+)',text) # Split at non alphanumeric sequences
	text=join(t,' ') # Join alphanumeric and non-alphanumeric, with spaces
	words=[ strip(w) for w in split(lower(text)) ] # Split tokens
	for word in words :
		yield token(word), word


# Test support
__test_program='''

dec square int x: int ->
    return x * x.
.

dec main ->
    square 2.
.

main.

'''

if __name__ == '__main__' :
    print [symbols[x][0] for x in symbols.keys()]
    for t,w in lexer(__test_program) :
        print t, w










