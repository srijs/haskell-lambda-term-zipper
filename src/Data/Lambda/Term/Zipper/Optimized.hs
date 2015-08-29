module Data.Lambda.Term.Zipper.Optimized where

import Safe

import qualified Data.Map as M

import Data.Lambda.Term
import Data.Lambda.Term.Zipper (Ctx(..))

type Env t = M.Map String [(t, Term t)]

push :: String -> t -> Term t -> Env t -> Env t
push key y t = M.insertWith (++) key [(y, t)]

pop :: String -> Env t -> Env t
pop = M.update tailMay

data Loc t = At { at :: Term t, ctx :: Ctx t, env :: Env t }

top :: Term t -> Loc t
top term = At term Top (M.empty)

down :: Loc t -> Loc t
down (At (Abs s y t1) ctx env) = At t1 (Abs' s y ctx) (push s y (Var s) env)
down (At (App t1 t2) ctx env) = At t1 (AppL ctx t2) env
down (At (Let s y t1 t2) ctx env) = At t1 (LetL s y ctx t2) env
down loc = loc

up :: Loc t -> Loc t
up (At t1 (Abs' s y ctx) env) = At (Abs s y t1) ctx (pop s env)
up (At t1 (AppL ctx t2) env) = At (App t1 t2) ctx env
up (At t2 (AppR t1 ctx) env) = At (App t1 t2) ctx env
up (At t1 (LetL s y ctx t2) env) = At (Let s y t1 t2) ctx env
up (At t2 (LetR s y t1 ctx) env) = At (Let s y t1 t2) ctx (pop s env)
up loc = loc

left :: Loc t -> Loc t
left (At t2 (AppR t1 ctx) env) = At t1 (AppL ctx t2) env
left (At t2 (LetR s y t1 ctx) env) = At t1 (LetL s y ctx t2) (pop s env)
left loc = loc

right :: Loc t -> Loc t
right (At t1 (AppL ctx t2) env) = At t2 (AppR t1 ctx) env
right (At t1 (LetL s y ctx t2) env) = At t2 (LetR s y t1 ctx) (push s y t1 env)
right loc = loc

lookup :: String -> Loc t -> Maybe (t, Term t)
lookup key (At _ _ env) = M.lookup key env >>= headMay
