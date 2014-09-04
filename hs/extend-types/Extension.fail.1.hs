import Base

type Principal = Int

data FacetTerm  = Facet Principal FacetTerm FacetTerm | BaseTerm Term
   deriving Show
--extend data Term = Facet Principal Term Term

data FacetValue = FacetV Principal FacetValue FacetValue | BaseValue Value
   deriving Show
--extend data Value = FacetV Principal Value Value

feval :: FacetTerm -> FacetValue
feval (Facet p t1 t2) = (FacetV p (feval t1) (feval t2))
feval (BaseTerm t) = (BaseValue (eval t))
--feval  = eval t

-- Tests

-- works
term0 = (Lam "x" Bot)
term1 = (Facet 0 (BaseTerm (Lam "x" Bot)) (BaseTerm Bot))

-- doesn't type
term2 = (Lam "y" (Facet 0 (BaseTerm (Lam "x" Bot)) (BaseTerm Bot)))
