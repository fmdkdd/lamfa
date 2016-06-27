#define plusdef function plus(a, b) { return a + b }

plusdef

console.log(plus(1, 2))

// GCC will try to be clever and guess the file type by looking
// at its extension.
//
// We can tell it it's a C file:
// gcc -E -xc preproc.js
//
// or we can hide the extension:
// gcc -E - < preproc.js
