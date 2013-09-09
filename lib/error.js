function InterpretError(message) {
  this.message = message;
  this.name = "InterpretError";
}

module.exports = {
  InterpretError: InterpretError,
};
