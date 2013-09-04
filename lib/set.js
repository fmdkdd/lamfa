function Set(elems) {
  this.elements = elems;
}

Set.prototype.has = function(elem) {
	return this.elements.indexOf(elem) > -1;
};

Set.prototype.add = function(elem) {
	return new Set(this.elements.concat(elem));
};

Object.defineProperty(Set.prototype, 'first', {
  get: function() {
	  return this.elements[0];
  }
});

Object.defineProperty(Set.prototype, 'rest', {
  get: function() {
	  return new Set(this.elements.slice(1));
  }
});

Object.defineProperty(Set.prototype, 'size', {
  get: function() {
    return this.elements.length;
  }
});

Set.prototype.isEmpty = function() {
	return this.size === 0;
};

module.exports = Set;
