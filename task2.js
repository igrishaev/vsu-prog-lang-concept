var OPERATORS = {
    '+': function(a, b) {return a + b;},
    '-': function(a, b) {return a - b;},
    '*': function(a, b) {return a * b;},
    '/': function(a, b) {return a / b;},
};

function isOneOf(values, chr) {
    return values.indexOf(chr) >= 0;
}

function isDigit(chr) {
    return isOneOf('0123456789.', chr);
}

function isOperator(chr) {
    return isOneOf('+-*/', chr);
}

function isPar(chr) {
    return isOneOf('()', chr);
}

function parseNumber(lexem) {
    var parser = (lexem.indexOf('.') >= 0) ? parseFloat : parseInt
    return parser(lexem);
}

function nextParIdx(lexems, i) {
    var lexem;
    var level = 0;
    while (typeof (lexem = lexems[i]) !== 'undefined') {
        if ('(' === lexem) {
            level++;
        }
        if (')' === lexem) {
            level--;
            if (level === 0) {
                return i;
            }
        }
        i++;
    }
    return -1;
}

function prevParIdx(lexems, i) {
    var lexem;
    var level = 0;
    while (typeof (lexem = lexems[i]) !== 'undefined') {
        if (')' === lexem) {
            level++;
        }
        if ('(' === lexem) {
            level--;
            if (level === 0) {
                return i;
            }
        }
        i--;
    }
    return -1;
}

function getLexems(expr) {
    var res = [];
    var operand = '';
    var chr;
    for (var i = 0; i <= expr.length; i++) {
        chr = expr[i];
        if (isDigit(chr)) {
            operand += chr;
        } else if (operand) {
            res.push(parseNumber(operand));
            operand = '';
        }
        if (isOperator(chr) || isPar(chr)) {
            res.push(chr);
        }
    }
    return res;
}

function addPars(lexems) {
    var lexem, nextPar, prevPar;
    var i = 0;
    while (typeof (lexem = lexems[i]) !== 'undefined') {
        if ('*' === lexem || '/' === lexem) {
            if (lexems[i + 1] === '(') {
                nextPar = nextParIdx(lexems, i + 1) + 1;
            } else {
                nextPar = i + 2;
            }
            lexems.splice(nextPar, 0, ')');

            if (lexems[i - 1] === ')') {
                prevPar = prevParIdx(lexems, i - 1);
            } else {
                prevPar = i - 1;
            }
            lexems.splice(prevPar, 0, '(');
            i++;
        }
        i++;
    }
    return lexems;
}

function evalLexems(lexems) {
    var operand, operator, prevPar, operatorPos;
    var len = lexems.length;
    if (lexems.length > 0) {
        operand = lexems[len - 1];
        operatorPos = len - 2;
        if (operand === ')') {
            prevPar = prevParIdx(lexems, len - 1);
            operand = evalLexems(lexems.slice(prevPar + 1, len - 1));
            operatorPos = prevPar - 1;
        }
        operator = lexems[operatorPos];
        if (operator) {
            return OPERATORS[operator](
                evalLexems(lexems.slice(0, operatorPos)), operand);
        }
        else {
            return operand;
        }
    }
}

function evalExpr(expr) {
    var lexems = getLexems(expr);
    addPars(lexems);
    return evalLexems(lexems);
}

// tests

function assert(predicate, msg) {
    if (!predicate) {
        throw msg || 'Assertion failed';
    }
}

function test(expr, expected) {
    console.log(expr + ' == ' + expected);
    var result = evalExpr(expr);
    assert(expected === result, result);
}

test('1', 1);
test('1 + 2', 3);
test('1 + 2 + 3', 6);
test('1 + 2 - 3 + 4', 4);
test('0 - 1 - 2 - 3 - 0', -6);

test('1 * 2', 2);
test('1 * 0', 0);
test('1 + 2 * 3', 7);
test('1 + 2 * 3 / 6', 2);
test('12 / 4 + 2 * 3', 9);

test('(3 + 2) * 4', 20);
test('(3 + 2) * (4 + 1)', 25);
test('20 / (3 + 2) * (4 + 1)', 20);
test('(((3 + 2) * 2) + 2) / 6', 2);

test('(((42))) + 1', 43);
test('((((((3))) + (((2))))))', 5);
test('(2) + (3) - (0) * (100)', 5)

test('1.2 + 2.1', 3.3)
test('0.1 + 0.01 + 0.001 + 0.0001', 0.1111)
test('0.1 * 50 / 0.1 - 5', 45)

console.log('Tests OK');
