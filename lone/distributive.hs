import Data.Functor.Const
import Data.Functor.Identity

class (Functor g) => Distributive g where
  distribute :: (Functor f) => f (g a) -> g (f a)

instance Distributive Identity where
  distribute :: (Functor f) => f (Identity a) -> Identity (f a)
  distribute gfa = Identity (runIdentity <$> gfa)

{-
instance Distributive ((,) b) where
  distribute :: Functor f => f (b, a) -> (b, f a)
  distribute = undefined

instance Distributive (Either b) where
  distribute :: Functor f => f (Either b a) -> Either b (f a)
  distribute = undefined

instance Distributive (Const b) where
  distribute :: Functor f => f (Const b a) -> Const b (f a)
  distribute fa = undefined
-}

instance Distributive ((->) b) where
  distribute :: (Functor f) => f (b -> a) -> b -> f a
  --    distribute ff b = (\f -> f b) <$> ff
  distribute ff b = ($ b) <$> ff

{-
class (Traversable f, Distributive f) => Identical f

instance Identical Identity
-}
type Identical f = (Traversable f, Distributive f)
