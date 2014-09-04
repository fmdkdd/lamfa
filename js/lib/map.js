function Map(obj) {
  obj.__proto__ = null;
  return obj;
}

module.exports = Map;
