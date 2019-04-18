module ProductCoProduct where 

{--
class Product s where
    fst :: s a b -> a
    snd :: s a b -> b

instance Product (,) where
    fst (x, _) = x
    snd (_, y) = y    

instance Product ((,,) a) where
    fst (_, x, _) = x
    snd (_, _, y) = y
--}


{--fst :: (a,b) -> a
fst (x,_) = x

snd :: (a,b) -> b
snd (_,y) = y
--}

{-- class (Span s) => Product s where
    pFactor :: (Span s') => s' a b -> s a b --}
    
{--
    λ:ProductCoProduct.fst (1,2)
    1

λ:ProductCoProduct.snd (1,2,3)
3

--}

