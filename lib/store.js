var Address = require('./address');

function Store(elements) {
  this.elements = elements || [];
}

Store.prototype.set = function(address, value) {
  var copy = this.elements.slice(0);
  copy[address.value] = value;
  return new Store(copy);
}

Store.prototype.get = function(address) {
  return this.elements[address.value];
}

Object.defineProperty(Store.prototype, 'size', {
  get: function() {
    return this.elements.length;
  },
});

module.exports = Store;
