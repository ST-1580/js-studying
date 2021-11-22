"use strict";

class Expression {
    constructor(evall, toStr, toDiff, pref, post) {
        this.evall = evall; this.toStr = toStr;
        this.toDiff = toDiff; this.pref = pref;
        this.post = post;
    }

    evaluate(x, y, z) {
        return this.evall(x, y, z);
    }

    toString() {
        return this.toStr;
    }

    diff(varr) {
        return this.toDiff(varr);
    }

    prefix() {
        return this.pref;
    }

    postfix() {
        return this.post;
    }
}

class Operation extends Expression {
    constructor(mode, operator, toDiff, args) {
        super( (x, y, z) => args.length == 0 ? 0 : mode(...args.map((a) => a.evaluate(x, y, z))),
                args.length == 0 ? " " : args.map((a) => a.toString() + " ").reduce((a, b) => a + b) + operator,
                (varr) => toDiff(varr, ...args),
                "(" + operator + (args.length == 0 ? " " : args.map((a) => " " + a.prefix()).reduce((a, b) => a + b)) + ")",
                "(" + (args.length == 0 ? " " : args.map((a) => a.postfix() + " ").reduce((a, b) => a + b)) + operator + ")");
    }
}

class Add extends Operation {
    constructor(...args) {
        super((...args) => args.reduce((a, b) => a + b), "+", (varr, a, b) => new Add(a.diff(varr), b.diff(varr)), args);
    }
}

class Subtract extends Operation {
    constructor(...args) {
        super((a, b) => a - b, "-", (varr, a, b) => new Subtract(a.diff(varr), b.diff(varr)), args);
    }
}

class Multiply extends Operation {
    constructor(...args) {
        super((...args) => args.reduce((a, b) => a * b), "*",
            (varr, a, b) => new Add(new Multiply(a.diff(varr), b), new Multiply(a, b.diff(varr))), args);
    }
}

class Divide extends Operation {
    constructor(...args) {
        super((a, b) => a / b, "/", (varr, a, b) =>
            new Divide(new Subtract(new Multiply(a.diff(varr), b), new Multiply(a, b.diff(varr))), new Multiply(b, b)), args);
    }
}

class Power extends Operation {
    constructor(...args) {
        super((a, b) => Math.pow(a, b), "pow", (varr, a, b) =>
            new Add(new Multiply(new Multiply(b, new Power(a, new Subtract(b, new Const(1)))), a.diff(varr)),
                new Multiply(new Multiply(new Power(a, b), new Log(new Const(Math.E), a)), b.diff(varr))), args);
    }
}

class Log extends Operation {
    constructor(...args) {
        super((a, b) => Math.log10(Math.abs(b)) / Math.log10(Math.abs(a)), "log", (varr, a, b) =>
            new Divide(new Subtract(new Multiply(new Divide(b.diff(varr), b), new Log(new Const(Math.E), a)),
                new Multiply(new Divide(a.diff(varr), a), new Log(new Const(Math.E), b))),
                new Multiply(new Log(new Const(Math.E), a), new Log(new Const(Math.E), a))), args);
    }
}

class Negate extends Operation {
    constructor(...args) {
        super((a) => -1 * a, "negate", (varr, a) => new Negate(a.diff(varr)), args);
    }
}

class Sumexp extends Operation {
    constructor(...args) {
        super((...args) => args.map((a) => Math.pow(Math.E, a)).reduce((a, b) => a + b), "sumexp", (varr, ...args) =>
            new Add(...args.map((a) => new Multiply(a.diff(varr), new Power(new Const(Math.E), a)))), args);
    }
}

class Softmax extends Operation {
    constructor(...args) {
        super((...args) => Math.pow(Math.E, args[0]) / args.map((a) => Math.pow(Math.E, a)).reduce((a, b) => a + b),
            "softmax", (varr, ...args) =>
                new Divide(new Subtract(new Multiply(new Sumexp(args[0]).diff(varr), new Sumexp(...args)),
                    new Multiply(new Sumexp(args[0]), new Sumexp(...args).diff(varr))), new Multiply(new Sumexp(...args), new Sumexp(...args))), args);
    }
}

class Const extends Expression {
    constructor(value) {
        super((x, y, z) => value, value.toString(), (varr) => new Const(0), value.toString(), value.toString())
    }
}

const Variables = new Map([
    ['x', 0],
    ['y', 1],
    ['z', 2]
]);

class Variable extends Expression {
    constructor(body) {
        super((...args) => args[Variables.get(body)], body, (varr) => varr === body ? new Const(1) : new Const(0), body, body);
    }
}

const Update = new Map([
    ['+', (...args) => new Add(...args)],
    ['-', (...args) => new Subtract(...args)],
    ['*', (...args) => new Multiply(...args)],
    ['/', (...args) => new Divide(...args)],
    ['pow', (...args) => new Power(...args)],
    ['log', (...args) => new Log(...args)],
    ['negate', (...args) => new Negate(...args)],
    ['sumexp', (...args) => new Sumexp(...args)],
    ['softmax', (...args) => new Softmax(...args)]
]);

const NumberOfOperands = new Map([
    ['+', 2],
    ['-', 2],
    ['/', 2],
    ['*', 2],
    ['negate', 1],
    ['log', 2],
    ['pow', 2],
    ['sumexp', -1],
    ['softmax', -1]
])


function parse(expression) {
    let j = 0;
    let stack = [];
    for (let i = 0; i < expression.length; i++) {
        if (expression[i] == " " || i == expression.length - 1) {
            let curr = expression.substring(j, i);
            if (i == expression.length - 1 && expression[i] != " ") {
                curr = expression.substring(j, i + 1);
            }
            j = i + 1;
            if (NumberOfOperands.has(curr)) {
                stack.push(Update.get(curr)(...stack.splice(-1 * NumberOfOperands.get(curr))));
            } else if (Variables.has(curr)) {
                stack.push(new Variable(curr));
            } else if (Number(curr)) {
                stack.push(new Const(Number(curr)));
            }
        }
    }
    return stack.pop();
}

// -------------------------------

class ParsingError extends Error {
    constructor(error) {
        super(error);
    }
}

let pos;
let left;
let balance;
let startPos;

function newStr(expression) {
    const newExpr = expression.replace(/\)/gi, ')  ');
    if (newExpr.length == 0) {
        throw new ParsingError("Empty input");
    }
    let to_parse = "(" + newExpr + ")";
    pos = 0;
    left = -1;
    balance = 0;
    return to_parse;
}

function make_curr(expression) {
    startPos = pos;
    let curr = expression.substring(left, pos);
    if (expression[pos] == "(") {
        let curr = expression.substring(left, pos - 1);
    } else if (pos == expression.length - 1 && expression[pos] != " " && expression[pos] != ")") {
        curr = expression.substring(left, pos + 1);
    }
    left = pos + 1;
    return curr;
}

function check(expression) {
    let delta = pos;
    while (pos < expression.length && expression[pos] == " ") {
        pos++;
    }
    delta = pos - delta;
    if (expression[pos] !== ')') {
        throw new ParsingError("error");
    } else if (delta != 0) {
        pos--;
        left = pos;
    }
}

const WrongExpressions = new Set([
    '(x)', '(y)', '(z)'
])

const Numbers = new Set([
    '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'
])

function parsePrefix(expression) {
    let ans = parsing(newStr(expression));
    if (balance > 0) {
        throw new ParsingError("Missing )");
    }
    if (balance < 0) {
        throw new ParsingError("Missing (");
    }
    return ans;
}

function parsePostfix(expression) {
    return parsePrefix(expression);
}

function make_error_str(expression) {
    const newExpr = expression.replace(/  /gi, '');
    const from_left = Math.max(0, startPos - 10);
    const to_right = Math.min(pos + 10, newExpr.length)
    return "..." + newExpr.substring(from_left, startPos) + "<----WRONG SYMBOL>" + newExpr.substring(startPos, to_right) + "...";
}

function parsing(expression) {
    let stack = [];
    let operator = "";
    const st = pos;
    while (pos < expression.length) {
        let curr;
        if (expression[pos] == " " || pos == expression.length - 1 || expression[pos] == ')' || expression[pos] == '(') {
            curr = make_curr(expression);
            if (NumberOfOperands.has(curr)) {
                if (operator != "") {
                    throw new ParsingError("A lot of operators in expression " + make_error_str(expression));
                }
                operator = curr;
            } else if (Number(curr)) {
                stack.push(new Const(Number(curr)));
            } else if (Variables.has(curr)) {
                stack.push(new Variable(curr));
            } else if (curr != "" && curr != " " && curr != ') ') {
                if (Numbers.has(curr)) {
                    throw new ParsingError("Wrong place of const in expression " + make_error_str(expression));
                }
                throw new ParsingError("Unknown symbol in expression " + make_error_str(expression));
            }
            if (expression[pos] == "(") {
                left = pos + 1;
                pos++; balance++;
                stack.push(parsing(expression));
            }
            if (expression[pos] == ")") {
                const currLvlExpression = expression.substring(st, pos - 2);
                if (WrongExpressions.has(currLvlExpression)) {
                    throw new ParsingError("Variable withut operations in expression " + make_error_str(expression));
                }
                balance--; left = pos + 1; pos++;
                if (operator != "") {
                    if (NumberOfOperands.get(operator) != -1) {
                        if (stack.length > NumberOfOperands.get(operator)) {
                            const cnt = NumberOfOperands.get(curr);
                            throw new ParsingError("A lot of operands of \'" + curr + "\' in expression " + make_error_str(expression) +
                                                                    " Expected " + cnt + ", found " + stack.length);
                        }
                        if (stack.length < NumberOfOperands.get(operator)) {
                            const cnt = NumberOfOperands.get(curr);
                            throw new ParsingError("A less of operands of \'" + curr + "\' in expression " + make_error_str(expression) +
                                                                    " Expected " + cnt + ", found " + stack.length);
                        }
                    }
                    stack.push(Update.get(operator)(...stack.splice(-1 * stack.length)));
                }
                if (stack.length != 1) {
                    throw new ParsingError("Expected operation symbol, but found \'" + curr + "\' in expression " + make_error_str(expression));
                }
                return stack.pop();
            }
        }
        pos++;
    }
    return stack.pop();
}

