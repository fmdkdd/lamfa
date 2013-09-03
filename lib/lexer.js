function lex(source) {
	var tokens = [];
	var i = 0;

  var undo = extendWith(String.prototype, lexStringUtils);

	var words = source
		.padAround(separators)
		.padAround(operators)
		.normalizeWhitespace()
		.trim()
		.collapseWhitespace()
		.split(' ');

  undo();

	var ret = words.map(tokenize);

  return ret;
}

var keywords = ['ref', 'read', 'write', 'true', 'false', 'if', 'fi', 'then', 'else', 'let', 'in', 'return', '⟂'];
var operators = /(:=|=|λ|!|\?|:)/g;
var separators = /(;|\(|\)|\[|\]|<|>|\.)/g;

keywords.contains = function(el) {
	return this.indexOf(el) > -1;
};

function isIdentifier(s) { return s.match(/^[$a-zA-Z0-9]+$/); }
function isNumber(s) { return s.match(/^[0-9]+$/); }
function isOperator(s) { return s.match(operators); }
function isSeparator(s) { return s.match(separators); }
function isKeyword(s) { return keywords.contains(s); }

function tokenize(word) {

	if (isIdentifier(word) && !isKeyword(word) && !isNumber(word))
		return token('identifier', word);
	if (isNumber(word))
		return token('number', word);
	if (isKeyword(word) || isOperator(word) || isSeparator(word))
		return token(word);
	else
		throw new Error('Unknown token ' + word);
}

function token(type, word) {
	var token = { type: type };

	if (word)
		token.value = word;

	return token;
}

function extendWith(obj, prototype) {
  prototype.__proto__ = obj.__proto__;
  obj.__proto__ = prototype;
  return function undo() {
    obj.__proto__ = prototype.__proto__;
  };
}

var lexStringUtils = {
  padAround: function(regex) {
	  return this.replace(regex, ' $1 ');
  },

  collapseWhitespace: function() {
	  return this.replace(/\s+/g, ' ');
  },

  normalizeWhitespace: function() {
	  return this.replace(/[\f\n\r\t\v]/g, ' ');
  },
};


// Module exports

module.exports = {
	lex: lex,
	tokenize: tokenize,
	token: token,
};
