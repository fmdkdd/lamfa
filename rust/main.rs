#[derive(Clone)]
enum Term {
  Num(u64),
  Plus(Box<Term>, Box<Term>),
}


fn main() {
  let e1 = Term::Num(3);
  let e2 = Term::Plus(Box::new(Term::Num(1)), Box::new(Term::Num(2)));

  {
    use eval1::Eval;
    println!("{} {}", e1.clone().eval(), e2.clone().eval());
  }

  {
    use eval2::Eval;
    println!("{} {}", e1.clone().eval(), e2.clone().eval());
  }

  {
    use eval3::Eval;
    println!("{} {}", e1.clone().eval(), e2.clone().eval());
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
