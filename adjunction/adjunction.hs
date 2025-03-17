{-# LANGUAGE FunctionalDependencies, InstanceSigs, MultiParamTypeClasses #-}

class (Functor l, Functor r) => Adjunction l r | l -> r, r -> l where
    unit :: a -> r (l a)
    counit :: l (r a) -> a

instance Adjunction ((,) e) ((->) e) where
    unit :: a -> (e -> (e, a))
    unit a e = (e, a)

    counit :: (e, e -> a) -> a
    counit (e, f) = f e
