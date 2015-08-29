module Data.Lambda.Term.Zipper where

import Prelude hiding (lookup)

import Data.Lambda.Term

data Ctx t = Top
           | Abs' String t (Ctx t)
           | AppL (Ctx t) (Term t)
           | AppR (Term t) (Ctx t)
           | LetL String t (Ctx t) (Term t)
           | LetR String t (Term t) (Ctx t)

data Loc t = At { at :: Term t, ctx :: Ctx t }

top :: Term t -> Loc t
top term = At term Top

down :: Loc t -> Loc t
down (At (Abs s y t1) ctx) = At t1 (Abs' s y ctx)
down (At (App t1 t2) ctx) = At t1 (AppL ctx t2)
down (At (Let s y t1 t2) ctx) = At t1 (LetL s y ctx t2)
down loc = loc

up :: Loc t -> Loc t
up (At t1 (Abs' s y ctx)) = At (Abs s y t1) ctx
up (At t1 (AppL ctx t2)) = At (App t1 t2) ctx
up (At t2 (AppR t1 ctx)) = At (App t1 t2) ctx
up (At t1 (LetL s y ctx t2)) = At (Let s y t1 t2) ctx
up (At t2 (LetR s y t1 ctx)) = At (Let s y t1 t2) ctx
up loc = loc

left :: Loc t -> Loc t
left (At t2 (AppR t1 ctx)) = At t1 (AppL ctx t2)
left (At t2 (LetR s y t1 ctx)) = At t1 (LetL s y ctx t2)
left loc = loc

right :: Loc t -> Loc t
right (At t1 (AppL ctx t2)) = At t2 (AppR t1 ctx)
right (At t1 (LetL s y ctx t2)) = At t2 (LetR s y t1 ctx)
right loc = loc

lookup :: String -> Loc t -> Maybe (t, Term t)
lookup key (At _ ctx) = lookup' ctx where
  lookup' Top = Nothing
  lookup' (Abs' s y ctx) | key == s = Just (y, Var s)
                         | otherwise = lookup' ctx
  lookup' (AppL ctx _) = lookup' ctx
  lookup' (AppR _ ctx) = lookup' ctx
  lookup' (LetL _ _ ctx _) = lookup' ctx
  lookup' (LetR s y t ctx) | key == s = Just (y, t)
                           | otherwise = lookup' ctx
