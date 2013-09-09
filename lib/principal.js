function Principal(label, negate) {
  if (typeof negate === 'undefined') {
    this.label = Math.abs(label);
    this.negated = label < 0;
  } else {
    this.label = label;
    this.negated = negate || false;
  }
}

Object.defineProperty(Principal.prototype, 'negation', {
  get: function() {
	  return new Principal(this.label, !this.isNegated());
  }
});

Principal.prototype.isNegated = function() {
	return !!this.negated;
};

module.exports = Principal;
