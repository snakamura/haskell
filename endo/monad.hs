{-# LANGUAGE TypeFamilies,
             UndecidableInstances
#-}

import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity(Identity))
import Data.Kind (Constraint, Type)

type f ~> g = forall a. f a -> g a

type MonoidF :: (Type -> Type) -> Constraint
class MonoidF f where
    type ProductF f :: Type -> Type
    type UnitF f :: Type -> Type
    muF :: ProductF f ~> f
    etaF :: UnitF f ~> f

instance MonoidF Maybe where
    type ProductF Maybe = Compose Maybe Maybe
    type UnitF Maybe = Identity
    muF (Compose (Just (Just x))) = Just x
    muF _ = Nothing
    etaF (Identity x) = Just x

instance MonoidF ((->) r) where
    type ProductF ((->) r) = Compose ((->) r) ((->) r)
    type UnitF ((->) r) = Identity
    muF (Compose f) e = f e e
    etaF (Identity x) = const x

newtype R r a = R (r -> a)

instance MonoidF (R r) where
    type ProductF (R r) = Compose (R r) (R r)
    type UnitF (R r) = Identity
    muF (Compose (R f)) = R $ \e -> let R g = f e in g e
    etaF (Identity x) = R $ const x

instance Monoid w => MonoidF ((,) w) where
    type ProductF ((,) w) = Compose ((,) w) ((,) w)
    type UnitF ((,) w) = Identity
    muF (Compose (w2, (w1, x))) = (w1 <> w2, x)
    etaF (Identity x) = (mempty, x)

newtype W w a = W (a, w)

instance Monoid w => MonoidF (W w) where
    type ProductF (W w) = Compose (W w) (W w)
    type UnitF (W w) = Identity
    muF (Compose (W (W (x, w1), w2))) = W (x, w1 <> w2)
    etaF (Identity x) = W (x, mempty)


class Functor f => Monad' f where
    join' :: f (f a) -> f a
    pure' :: a -> f a

instance (Functor f, MonoidF f, ProductF f ~ Compose f f, UnitF f ~ Identity) => Monad' f where
    join' = muF . Compose
    pure' = etaF . Identity

instance (Functor f, MonoidF f, ProductF f ~ Compose f f, UnitF f ~ Identity) => Applicative f where
--    (<*>) f m = f >>= \f' -> m >>= \m' -> pure (f' m')
--    (<*>) f m = (>>=) f (\f' -> m >>= \m' -> pure (f' m'))
--    (<*>) f m = muF'' $ Compose $ fmap (\f' -> m >>= \m' -> pure (f' m')) f
--    (<*>) f m = muF'' $ Compose $ fmap (\f' -> (>>=) m (\m' -> pure (f' m'))) f
--    (<*>) f m = muF'' $ Compose $ fmap (\f' -> muF'' $ Compose $ fmap (\m' -> pure (f' m')) m) f
    (<*>) f m = muF $ Compose $ fmap (\f' -> muF $ Compose $ fmap (pure . f') m) f
    pure = etaF . Identity

instance (Functor f, MonoidF f, ProductF f ~ Compose f f, UnitF f ~ Identity) => Monad f where
    (>>=) m f = muF $ Compose $ fmap f m
