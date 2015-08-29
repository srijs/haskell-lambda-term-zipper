module Data.Lambda.Term where

data Term t = Var String
            | Abs String t (Term t)
            | App (Term t) (Term t)
            | Let String t (Term t) (Term t)
