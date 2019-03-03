module Modeling.Data.Bidi where

import Control.Monad ((>=>))
import Data.Void (Void, absurd)
import Modeling.Data.Fix (Fix (..))

-- TODO are these better expressed with prisms and isos?


data Bijection a b = Bijection { biApply :: a -> b, biInvert :: b -> a }

postTraverseBijection :: Traversable f => Bijection b c -> Bijection a (f b) -> Bijection a (f c)
postTraverseBijection (Bijection innerApl innerInj) (Bijection outerApl outerInj) = Bijection apl inj where
    apl = (innerApl <$>) . outerApl
    inj = outerInj . (innerInj <$>)

preTraverseBijection :: Traversable f => Bijection z a -> Bijection (f a) b -> Bijection (f z) b
preTraverseBijection (Bijection innerApl innerInj) (Bijection outerApl outerInj) = Bijection apl inj where
    apl = outerApl . (innerApl <$>)
    inj = (innerInj <$>) . outerInj

idBijection :: Bijection a a
idBijection = Bijection id id

composeBijection :: Bijection b c -> Bijection a b -> Bijection a c
composeBijection (Bijection apl1 inv1) (Bijection apl2 inv2) = Bijection (apl1 . apl2) (inv2 . inv1)

flipBijection :: Bijection a b -> Bijection b a
flipBijection (Bijection apl inv) = Bijection inv apl

voidBijection :: Bijection a b -> Injection Void a b
voidBijection (Bijection apl inv) = Injection apl (Right . inv)

lowerBijection :: Bijection a b -> Injection e a b
lowerBijection = injectionMapError absurd . voidBijection

preFixBijection :: Bijection (f (Fix f)) b -> Bijection (Fix f) b
preFixBijection (Bijection innerApl innerInj) = Bijection (innerApl . unFix) (Fix . innerInj)

postFixBijection :: Bijection a (f (Fix f)) -> Bijection a (Fix f)
postFixBijection (Bijection innerApl innerInj) = Bijection (Fix . innerApl) (innerInj . unFix)

data Injection e a b = Injection { injApply :: a -> b, injInvert :: b -> Either e a }

postTraverseInjection :: Traversable f => Injection e b c -> Injection e a (f b) -> Injection e a (f c)
postTraverseInjection (Injection innerApl innerInj) (Injection outerApl outerInj) = Injection apl inj where
    apl = (innerApl <$>) . outerApl
    inj = traverse innerInj >=> outerInj

preTraverseInjection :: Traversable f => Injection e z a -> Injection e (f a) b -> Injection e (f z) b
preTraverseInjection (Injection innerApl innerInj) (Injection outerApl outerInj) = Injection apl inj where
    apl = outerApl . (innerApl <$>)
    inj = outerInj >=> traverse innerInj

preFixInjection :: Injection e (f (Fix f)) b -> Injection e (Fix f) b
preFixInjection (Injection innerApl innerInj) = Injection (innerApl . unFix) ((Fix <$>) . innerInj)

postFixInjection :: Injection e a (f (Fix f)) -> Injection e a (Fix f)
postFixInjection (Injection innerApl innerInj) = Injection (Fix . innerApl) (innerInj . unFix)

-- Category without the id conflict
idInjection :: Injection e a a
idInjection = Injection id Right

composeInjection :: Injection e b c -> Injection e a b -> Injection e a c
composeInjection (Injection apl1 inv1) (Injection apl2 inv2) = Injection (apl1 . apl2) (inv1 >=> inv2)

injectionMapError :: (e -> e') -> Injection e a b -> Injection e' a b
injectionMapError f (Injection apl oldInv) = Injection apl newInv where
    newInv b =
        case oldInv b of
            Left e -> Left (f e)
            Right a -> Right a

composeLeft :: Bijection b c -> Injection e a b -> Injection e a c
composeLeft (Bijection apl1 inv1) (Injection apl2 inv2) = Injection (apl1 . apl2) (inv2 . inv1)

composeRight :: Injection e b c -> Bijection a b -> Injection e a c
composeRight (Injection apl1 inv1) (Bijection apl2 inv2) = Injection (apl1 . apl2) ((inv2 <$>) . inv1)
