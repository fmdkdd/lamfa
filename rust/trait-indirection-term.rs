trait Eval {
  fn eval(self) -> u64;
}

fn main() {
  {
    use eval1::{num, plus};
    println!("{}", plus(num(1), num(2)).eval());
  }

  {
    use eval2::{num, plus};
    println!("{}", plus(num(1), num(2)).eval());
  }

  // {
  //   use eval3::Eval;
  //   println!("{}", plus(num(1), num(2)).eval());
  // }
}

mod eval1 {
  pub enum Term {
    Num(u64),
    Plus(Box<Term>, Box<Term>),
  }

  pub fn num(n: u64) -> Term {
    Term::Num(n)
  }

  pub fn plus(a: Term, b: Term) -> Term {
    Term::Plus(Box::new(a), Box::new(b))
  }

  use Eval;
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
  pub enum Term {
    Num(u64),
    Plus(Box<Term>, Box<Term>),
  }

  pub fn num(n: u64) -> Term {
    Term::Num(n)
  }

  pub fn plus(a: Term, b: Term) -> Term {
    Term::Plus(Box::new(a), Box::new(b))
  }

  use Eval;
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
  pub enum Term {
    Num(u64),
    Plus(Box<Term>, Box<Term>),
  }

  pub fn num(n: u64) -> Term {
    Term::Num(n)
  }

  pub fn plus(a: Term, b: Term) -> Term {
    Term::Plus(Box::new(a), Box::new(b))
  }

  use Eval;
  impl Eval for Term {
    fn eval(self) -> u64 {
      match self {
        // Argh! Cannot mix terms!
        Term::Num(_) => eval1::Eval(self) * 2,
        Term::Plus(t1, t2) => t1.eval() + t2.eval(),
      }
    }
  }
}
