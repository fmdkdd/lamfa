type Principal = Int

data Term = Facet Principal Term Term
data Value = FacetV Principal Value Value

eval (Facet p t1 t2) = FacetV p (eval t1) (eval t2)
