module Modeling.Data.Bidi where

import Control.Monad ((>=>))
import Data.Void (Void, absurd)
-- import Modeling.Data.Fix (Fix (..))

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

-- fixBijection :: Bijection (f (Fix f)) (Fix f)
-- fixBijection = Bijection Fix unFix

-- preFixBijection :: Bijection (f (Fix f)) b -> Bijection (Fix f) b
-- preFixBijection = flip composeBijection (flipBijection fixBijection)

-- preUnFixBijection :: Bijection (Fix f) b -> Bijection (f (Fix f)) b
-- preUnFixBijection = flip composeBijection (fixBijection)

-- postFixBijection :: Bijection a (f (Fix f)) -> Bijection a (Fix f)
-- postFixBijection = composeBijection fixBijection

-- postUnFixBijection :: Bijection a (Fix f) -> Bijection a (f (Fix f))
-- postUnFixBijection = composeBijection (flipBijection fixBijection)

-- bothFixBijection :: Bijection (f (Fix f)) (g (Fix g)) -> Bijection (Fix f) (Fix g)
-- bothFixBijection = preFixBijection . postFixBijection

-- bothUnFixBijection :: Bijection (Fix f) (Fix g) -> Bijection (f (Fix f)) (g (Fix g))
-- bothUnFixBijection = preUnFixBijection . postUnFixBijection

data Injection e a b = Injection { injApply :: a -> b, injInvert :: b -> Either e a }

postTraverseInjection :: Traversable f => Injection e b c -> Injection e a (f b) -> Injection e a (f c)
postTraverseInjection (Injection innerApl innerInj) (Injection outerApl outerInj) = Injection apl inj where
    apl = (innerApl <$>) . outerApl
    inj = traverse innerInj >=> outerInj

preTraverseInjection :: Traversable f => Injection e z a -> Injection e (f a) b -> Injection e (f z) b
preTraverseInjection (Injection innerApl innerInj) (Injection outerApl outerInj) = Injection apl inj where
    apl = outerApl . (innerApl <$>)
    inj = outerInj >=> traverse innerInj

-- preFixInjection :: Injection e (f (Fix f)) b -> Injection e (Fix f) b
-- preFixInjection = flip composeRight (flipBijection fixBijection)

-- postFixInjection :: Injection e a (f (Fix f)) -> Injection e a (Fix f)
-- postFixInjection = composeLeft fixBijection

-- bothFixInjection :: Injection e (f (Fix f)) (g (Fix g)) -> Injection e (Fix f) (Fix g)
-- bothFixInjection = preFixInjection . postFixInjection

-- preUnFixInjection :: Injection e (Fix f) b -> Injection e (f (Fix f)) b
-- preUnFixInjection = flip composeRight fixBijection

-- postUnFixInjection :: Injection e a (Fix g) -> Injection e a (g (Fix g))
-- postUnFixInjection = composeLeft (flipBijection fixBijection)

-- bothUnFixInjection :: Injection e (Fix f) (Fix g) -> Injection e (f (Fix f)) (g (Fix g))
-- bothUnFixInjection = preUnFixInjection . postUnFixInjection

voidInjection :: Injection Void a b -> Bijection a b
voidInjection (Injection innerApl innerInj) = Bijection innerApl inj where
    inj e =
        case innerInj e of
            Right a -> a

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

raiseInjection :: (e -> b) -> Injection e a b -> Bijection (Either e a) b
raiseInjection f (Injection apl inj) = Bijection (either f apl) inj
