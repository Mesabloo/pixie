{-# LANGUAGE TypeOperators, MultiParamTypeClasses, RankNTypes, ConstraintKinds, AllowAmbiguousTypes
           , FlexibleContexts, TypeApplications, ScopedTypeVariables, FlexibleInstances #-}

module Constraint.Unions
(type (.|), type (?), MaybeC, resolve, given) where

class c .| d where
    resolve :: (c => r) -> (d => r) -> r
infixr 2 .|

inLeft :: forall c d r. c => (c => r) -> (d => r) -> r
inLeft r _ = r

inRight :: forall c d r. d => (c => r) -> (d => r) -> r
inRight _ r = r

instance ((c0 .| d), (c1 .| d)) => (c0, c1) .| d where
    resolve = resolve @c0 @d (resolve @c1 @d inLeft inRight) inRight

instance ((c0 .| d), ((c1,c2) .| d)) => (c0, c1, c2) .| d where
    resolve = resolve @c0 @d (resolve @(c1,c2) @d inLeft inRight) inRight

type MaybeC c = c .| ()

type p? a = MaybeC (p a)

given :: forall c r. MaybeC c => (c => r) -> r -> r
given = resolve @c @() inJust inNothing
  where
    inJust :: forall c r. c => (c => r) -> r -> r
    inJust r _ = r

    inNothing :: forall c r. (c => r) -> r -> r
    inNothing _ r = r