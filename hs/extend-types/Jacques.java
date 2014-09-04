// data Expr = Lit Int | Bin Expr Expr

// data XExpr = UnExpr Expr

// eval :: Expr -> Int
// eval (Lit x) = x
// eval (Bin e1 e2) = eval e1 + eval e2

// xeval :: XExpr -> Int
// xeval (UnExpr e) = - eval e
// xeval e = eval e

// Syntaxe de base :

// public cclass AST {
// 	abstract public cclass Expr {
// 	}
// 	public cclass BinExpr extends Expr {
// 		public Expr expr1;
// 		public Expr expr2;

// 		public BinExpr(Expr expr1, Expr expr2){
// 			this.expr1 = expr1;
// 			this.expr2 = expr2;
// 		}
// 	}
// 	public cclass Lit extends Expr {
// 		public int val;

// 		public Lit(int val){
// 			this.val = val;
// 		}
// 	}
// }

// Interpréteur :

// public cclass Eval extends AST {
// 	abstract public cclass Expr {
// 		abstract public int eval();
// 	}
// 	public cclass BinExpr {
// 		public int eval() {
// 			return expr1.eval() + expr2.eval();
// 		}
// 	}
// 	public cclass Lit {
// 		public int eval() {
// 			return val;
// 		}
// 	}
// }

// Syntaxe étendue :

// public cclass XAST extends AST {
//     public cclass UnExpr extends Expr {
// 	public Expr expr;

//         public UnExpr(Expr expr){
// 	    this.expr = expr;
// 	}
//     }
// }

// Interpréteur étendu :

// public cclass XEval extends Eval & XAST {
//     public cclass UnExpr {
// 	public int eval() {
// 	    return - expr.eval();
// 	}
//     }
// }

// Test :

// 	final Eval eval = new Eval();
// 	eval.Expr expr1 = eval.new Lit(1);
// 	eval.Expr expr2 = eval.new BinExpr(expr1, expr1);
// 	eval.Expr expr3 = eval.new BinExpr(expr1, expr2);
// 	System.out.println(expr3.eval());

// 	final XEval xEval = new XEval();
// 	xEval.Expr xExpr1 = xEval.new Lit(1);
// 	xEval.Expr xExpr2 = xEval.new UnExpr(xExpr1);
// 	xEval.Expr xExpr3 = xEval.new BinExpr(xExpr1, xExpr2);
// 	System.out.println(xExpr3.eval());

interface Expr { int eval(); }

class BinExpr implements Expr {
    Expr e1, e2;
    public BinExpr(Expr e1, Expr e2) { this.e1 = e1; this.e2 = e2; }
    public int eval() { return e1.eval() + e2.eval(); }
}

class Lit implements Expr {
    int val;
    public Lit(int val) { this.val = val; }
    public int eval() { return this.val; }
}

class UnExpr implements Expr {
    Expr e;
    public UnExpr(Expr e) { this.e = e; }
    public int eval() { return -this.e.eval(); }
}

class Jacques {
    public static void main(String[] args) {
        Expr e1 = new Lit(1);
        Expr e2 = new BinExpr(e1, e1);
        Expr e3 = new BinExpr(e1, e2);
        System.out.println(e3.eval());

        Expr xe1 = new Lit(1);
        Expr xe2 = new UnExpr(xe1);
        Expr xe3 = new BinExpr(xe1, xe2);
        System.out.println(xe3.eval());
    }
}
