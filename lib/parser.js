var nodes = {
  constant: nodeMaker('constant', 'value'),
  variable: nodeMaker('variable', 'string'),
  abstraction: nodeMaker('abstraction', 'variable', 'body'),
  application: nodeMaker('application', 'e1', 'e2'),
  reference: nodeMaker('reference', 'expr'),
  dereference: nodeMaker('dereference', 'expr'),
  assignment: nodeMaker('assignment', 'left', 'right'),
  sequence: nodeMaker('sequence', 'left', 'right'),
  fiThen: nodeMaker('fi-then', 'condition', 'thenBranch'),
  ifThen: nodeMaker('if-then', 'condition', 'thenBranch'),
  ifThenElse: nodeMaker('if-then-else', 'condition', 'thenBranch', 'elseBranch'),
  letIn: nodeMaker('let-in', 'variable', 'varBody', 'body'),
  true: nodeMaker('true'),
  false: nodeMaker('false'),
  bottom: nodeMaker('⟂'),
  facetExpr: nodeMaker('facetExpr', 'principal', 'high', 'low'),
};

function parse(tokens) {
  var nextToken;

  function advance() {
    nextToken = tokens.shift() || { type: 'end' };
  }

  function expect(terminal) {
    if (nextToken.type == terminal) {
      var token = nextToken;
      advance();
      return token;
    } else
      throw new Error('Expected token "' + terminal + '" got "' + nextToken.type + '"');
  }

  // Terminals

  function variable() {
    var node = nodes.variable(expect('identifier').value);
    operandStack.push(node);
    return node;
  }

  function constant() {
    var node = nodes.constant(parseInt(expect('number').value));
    operandStack.push(node);
    return node;
  }

  function bottom() {
    expect('⟂');
	  var node = nodes.bottom();
    operandStack.push(node);
    return node;
  }

  function booleanTrue() {
    expect('true');
    var node = nodes.true()
    operandStack.push(node);
    return node;
  }

  function booleanFalse() {
    expect('false');
    var node = nodes.false();
    operandStack.push(node);
    return node;
  }

  // Non-terminals

  function simpleExpression() {
    if (nextToken.type == 'identifier') {
      return variable();
    }

    if (nextToken.type == 'number') {
      return constant();
    }

    if (nextToken.type == '⟂') {
      return bottom();
    }

    if (nextToken.type == '[') {
      expect('[');

      var e1 = expression();

      var e2 = expression();

      expect(']');

      while (precedence(']') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push(']');
      unstackOperator();

      var node = nodes.application(e1, e2);

      return node;
    }

    if (nextToken.type == '(') {
      expect('(');

      operatorStack.push('(');

      var e = simpleExpression();

      expect(')');

      while (operatorStack.peek() != '(')
        unstackOperator();
      unstackOperator();    // Remove useless '(' from stack

      return e;
    }

    if (nextToken.type == '<') {
      expect('<');

      var p = constant();

      expect('?');

      var h = simpleExpression();

      expect(':');

      var l = simpleExpression();

      expect('>');

      while (precedence('>') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('>');
      unstackOperator();

      var node = nodes.facetExpr(p, h, l);

      return node;
    }

    if (nextToken.type == '!') {
      expect('!');

      while (precedence('!') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('!');

      var e = expression();

      var node = nodes.dereference(e);

      return node;
    }

    if (nextToken.type == 'ref') {
      expect('ref');

      while (precedence('ref') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('ref');

      var e = expression();

      var node = nodes.reference(e);

      return node;
    }

    if (nextToken.type == 'λ') {
      expect('λ');

      while (precedence('λ') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('λ');

      var v = variable();

      expect('.');

      while (precedence('.') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('.');

      var e = expression();

      var node = nodes.abstraction(v, e);

      return node;
    }

    if (nextToken.type == 'if') {
      expect('if');

      while (precedence('if') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('if');

      var cond = simpleExpression();

      expect('then');

      while (precedence('then') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('then');

      var thenBranch = expression();

      var node = nodes.ifThen(cond, thenBranch);

      return node;
    }

    if (nextToken.type == 'fi') {
      expect('fi');

      while (precedence('fi') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('fi');

      var cond = simpleExpression();

      expect('then');

      while (precedence('then') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('then');

      var thenBranch = expression();

      var node = nodes.fiThen(cond, thenBranch);

      return node;
    }

    if (nextToken.type == 'let') {
      expect('let');

      while (precedence('let') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('let');

      var v = variable();
      expect('=');
      var e1 = expression();
      expect('in');

      while (precedence('in') < precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push('in');

      var e2 = expression();

      var node = nodes.letIn(v, e1, e2);

      return node;
    }

    if (nextToken.type == 'true') {
      return booleanTrue();
    }

    if (nextToken.type == 'false') {
      return booleanFalse();
    }

    throw new SyntaxError('Unexpected token "' + nextToken.type + '"');
  }

  function expression() {
    var s = simpleExpression();

    if (nextToken.type == ':=') {
      expect(':=');

      while (precedence(':=') <= precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push(':=');

      var right = expression();

      return nodes.assignment(s, right);
    }

    if (nextToken.type == ';') {
      expect(';');

      while (precedence(';') <= precedence(operatorStack.peek())) {
        unstackOperator();
      }

      operatorStack.push(';');

      var e = expression();

      return nodes.sequence(s, e);
    }

    return s;
  }

  var operatorStack = [];
  operatorStack.push('sentinel');
  var operandStack = [];

  // Operators

  function precedence(op) {
    var precedences = ['sentinel', '(', ';', 'let', 'in', 'fi', 'if', 'then',
                       'λ', '.', ']', '>', ':=', '!', 'ref'];
    var index = precedences.indexOf(op);

    if (index == -1)
      throw new Error('Operator without precedence ' + op);
    return index;
  }

  function unstackOperator() {
    var op = operatorStack.pop();

    switch (op) {
    case ';':
      var right = operandStack.pop();
      var left = operandStack.pop();

      if (!left || !right)
        throw new ParseError('Undefined operand in `;`', left, right);

      operandStack.push(nodes.sequence(left, right));
      break;

    case 'if':
      var thenBranch = operandStack.pop();
      var condition = operandStack.pop();

      if (!condition || !thenBranch)
        throw new ParseError('Undefined operand in `if`', condition, thenBranch);

      operandStack.push(nodes.ifThen(condition, thenBranch));
      break;

    case 'fi':
      var thenBranch = operandStack.pop();
      var condition = operandStack.pop();

      if (!condition || !thenBranch)
        throw new ParseError('Undefined operand in `fi`', condition, thenBranch);

      operandStack.push(nodes.fiThen(condition, thenBranch));
      break;

    case 'then':
      break;

    case 'let':
      var body = operandStack.pop();
      var varBody = operandStack.pop();
      var variable = operandStack.pop();

      if (!variable || !varBody || !body)
        throw new ParseError('Undefined operand in `let`', variable, varBody, body);

      operandStack.push(nodes.letIn(variable, varBody, body));
      break;

    case 'in':
      break;

    case ':=':
      var right = operandStack.pop();
      var left = operandStack.pop();

      if (!left || !right)
        throw new ParseError('Undefined operand in `:=`', left, right);

      operandStack.push(nodes.assignment(left, right));
      break;

    case 'λ':
      var body = operandStack.pop();
      var variable = operandStack.pop();

      if (!variable || !body)
        throw new ParseError('Undefined operand in `λ`', variable, body);

      operandStack.push(nodes.abstraction(variable, body));
      break;

    case '.':
      break;

    case '(':
      break;

    case '[':
      break;

    case ']':
      var e2 = operandStack.pop();
      var e1 = operandStack.pop();

      if (!e1 || !e2)
        throw new ParseError('Undefined operand in `]`', e1, e2);

      operandStack.push(nodes.application(e1, e2));
      break;

    case '>':
      var l = operandStack.pop();
      var h = operandStack.pop();
      var p = operandStack.pop();

      if (!p || !h || !l)
        throw new ParseError('Undefined operand in `>`', p, h, l);

      operandStack.push(nodes.facetExpr(p, h, l));
      break;

    case '!':
      var e = operandStack.pop();

      if (!e)
        throw new ParseError('Undefined operand in `!`', e);

      operandStack.push(nodes.dereference(e));
      break;

    case 'ref':
      var e = operandStack.pop();

      if (!e)
        throw new ParseError('Undefined operand in `ref`', e);

      operandStack.push(nodes.reference(e));
      break;

    case 'sentinel':
      throw new Error('Cannot unstack sentinel');
      break;

    default:
      throw new Error('Unknown operator ' + op);
    }
  }

  function ParseError() {
    var args = Array.prototype.slice.call(arguments);
    var message = args.map(prettyPrint).join(' ');

    function prettyPrint(arg) {
      if (typeof arg == 'object')
        return JSON.stringify(arg);
      if (typeof arg == 'undefined')
        return 'undefined';
      else
        return arg;
    }

    var parserState = {
      nextToken: nextToken,
      operandStack: operandStack,
      operatorStack: operatorStack,
    };

    this.message = message + '\n' + JSON.stringify(parserState, null, 2);
    this.name = "ParseError";
  }

  advance();
  var CST = expression();

  while (operatorStack.peek() != 'sentinel')
    unstackOperator();

  var AST = operandStack;

  return AST[0];
}

function objectCreator() {
  var names = Array.prototype.slice.call(arguments);

  return function() {
    var args = Array.prototype.slice.call(arguments);

    var o = {};
    for (var i in names) {
      o[names[i]] = args[i];
    }

    return o;
  }
}

function nodeMaker(type) {
  var rest = Array.prototype.slice.call(arguments, 1);
  rest.unshift('type');
  return objectCreator.apply(this, rest).bind(undefined, type);
}

Object.defineProperty(Array.prototype, 'peek', {
  value: function() {
    return this[this.length - 1];
  },
  configurable: true,
  enumerable: false,
  writeable: true,
});

module.exports = {
  parse: parse,
  nodes: nodes,
}
