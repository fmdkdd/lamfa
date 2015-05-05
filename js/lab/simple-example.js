var base = (function(){

  function constant(n) {
    return {
      eval() {return n;}
    };
  }

  function plus(e1, e2) {
    return {
      eval() {return e1.eval() + e2.eval();}
    };
  }

  function interpret(ast) {
    return ast.eval();
  }

  return {constant, plus, interpret};

}());

with (base) {
  interpret(plus(constant(1), constant(2)));
} //: 3


// New variant

var newvariant = (function(){

  function pair(a,b) {
    return {
      eval() {return [a,b];}
    };
  }

  function plus(e1, e2) {
    return {
      eval() {
        var v1 = e1.eval();
        var v2 = e2.eval();
        if (isPair(v1) && isPair(v2))
          return [v1[0] + v2[0], v1[1] + v2[1]];
        else
          return v1 + v2;
      }
    };
  }

  function isPair(v) {return Array.isArray(v);}

  return {pair, plus};
}());

with (base) {
  with (newvariant) {
    interpret(plus(pair(1,1), pair(2,2)));
  }
} //: [3,3]


// New operation

var show = function(base){

  function constant(n) {
    var b = base.constant(n);
    b.show = function() {return n;};
    return b;
  }


  function plus(e1, e2) {
    var b = base.plus(e1, e2);
    b.show = function() {return e1.show() + ' + ' + e2.show();};
    return b;
  };

  function show(ast) {
    return ast.show();
  }

  return {constant, plus, show};

};

with (show(base)) {
  show(plus(constant(1), constant(2)));
} //: "1 + 2"

with (base) {
  with (show(base)) {
    interpret(plus(constant(1), constant(2)));
  }
} //: 3



// Modify operation

var modify = (function(){

  function constant(n) {
    return {
      eval() {return 2 * n;}
    };
  }

  return {constant};

}());

with (base) {
  with (modify) {
    interpret(plus(constant(1), constant(2)));
  }
} //: 6


// Pass some context down

var context = function(base){

  function constant(n) {
    return {
      eval() {
        pc++;
        return base.constant(n).eval();
      }
    };
  }

  function plus(e1,e2) {
    return {
      eval() {
        pc++;
        return base.plus(e1,e2).eval();
      }
    };
  }

  var pc;

  function interpret(ast) {
    pc = 0;
    var v = ast.eval();
    return [pc, v];
  }

  return {constant, plus, interpret};

};

with (context(base)) {
  interpret(plus(constant(1), constant(2)));
} //: [3,3]
