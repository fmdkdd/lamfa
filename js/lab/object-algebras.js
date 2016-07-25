/* eslint-disable */

/// Object algebras
// My attempt at understanding this talk:
// https://www.youtube.com/watch?v=snbsYyBS4Bs

function someItemPath(algebra) {
  with (algebra) {
    return chained(segment("item"), segment("123abc"))
  }
}

var pathClient = {
  segment(value) { return encodeURIComponent(value) },
  chained(first, second) { return `${first}/${second}` },
}

someItemPath(pathClient) //: "item/123abc"

var pathRouting = {
  segment(value) { return incoming => decodeURIComponent(incoming) == value },
  chained(first, second) {
    return incoming => {
      var [hd,...tl] = incoming.split('/')
      tl = tl.join('/')
      return tl && first(hd) && second(tl)
    }
  }
}

var isSomeItem = someItemPath(pathRouting) //: undefined
isSomeItem('foo/bar') //: false
isSomeItem('item/123abc') //: true

// So, actually it looks /a lot/ like what I did for FOAL.  The main difference
// being that I composed the algebras using `with`.  And instead of creating a
// result through a function like ~someItemPath~, I used the value immediately.
