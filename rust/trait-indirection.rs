#[derive(Clone)]
enum Term {
  Num(u64),
  Plus(Box<Term>, Box<Term>),
}

fn num(n: u64) -> Term {
  Term::Num(n)
}

fn plus(a: Term, b: Term) -> Term {
  Term::Plus(Box::new(a), Box::new(b))
}

fn main() {
  {
    use eval1::Eval;
    println!("{} ", num(3).eval());
    println!("{} ", plus(num(1), num(2)).eval());
  }

  {
    use eval2::Eval;
    println!("{} ", num(3).eval());
    println!("{} ", plus(num(1), num(2)).eval());
  }

  {
    use eval3::Eval;
    println!("{} ", num(3).eval());
    println!("{} ", plus(num(1), num(2)).eval());
  }
}

mod eval1 {
  use Term;

  pub trait Eval {
    fn eval(self) -> u64;
  }

  impl Eval for Term {
    fn eval(self) -> u64 {
      match self {
        Term::Num(n) => n,
        Term::Plus(t1, t2) => t1.eval() + t2.eval(),
      }
    }
  }
}

mod eval2 {
  use Term;

  pub trait Eval {
    fn eval(self) -> u64;
  }

  impl Eval for Term {
    fn eval(self) -> u64 {
      match self {
        Term::Num(n) => n * 2,
        Term::Plus(t1, t2) => t1.eval() + t2.eval(),
      }
    }
  }
}

mod eval3 {
  use eval1;
  use Term;

  pub trait Eval {
    fn eval(self) -> u64;
  }

  impl Eval for Term {
    fn eval(self) -> u64 {
      match self {
        Term::Num(_) => eval1::Eval::eval(self) * 2,
        Term::Plus(t1, t2) => t1.eval() + t2.eval(),
      }
    }
  }
}
