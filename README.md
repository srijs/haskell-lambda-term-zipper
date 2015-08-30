# Functional Pearl: Environmentally Aware Zippers

## Introduction

Suppose you wanted to implement an algorithm that traverses and rewrites a program term of some type. If said term includes a way to bind variables to other terms inside an environment, you are now faced with the task of keeping track of the bound environment as you traverse the tree.

When implementing algorithms related to the lambda calculus, e.g performing any form of alpha-conversion, beta-reduction, type checking or type inference, this is a common scenario. It is common enough that it warrants a general solution, which will be presented in this pearl.

The following (excerpt of a) term type will serve us as an example throughout the chapters.

    data Term t = Var String
                | Abs String t (Term t)
                | App (Term t) (Term t)
                | Let String t (Term t) (Term t)
            
The type variable `t` can serve as a carrier for arbitrary bits of information, but is designed to be used for embedding typing information within the term. The set of untyped terms is `Term ()`, while `Term Type` describes the set of fully-typed terms where the type `Type` captures the typing information. A partially typed term would be `Term (Maybe Type)`, and so forth.

Beta-reduction is performed such that a term `App (Abs s y t1) t2` is reduced to `Let s y t1 t2`, which represents `t2[s := t1]`.

Before we continue, credit has to be given to Hinze and Jeuring's excellent pearl _Weaving a Web_, from which the following data types are derived.[1]

## The Zipper

In order to be able to navigate this tree easily, let us introduce two data types and a few related functions.

    data Loc t = At { it :: Term t, ctx :: Ctx t }

    data Ctx t = Top
               | Abs' String t (Ctx t)
               | AppL (Ctx t) (Term t)
               | AppR (Term t) (Ctx t)
               | LetL String t (Ctx t) (Term t)
               | LetR String t (Term t) (Ctx t)

This context data type is structurally very similar to the `Term` type introduced earlier. It is, in fact, the derivate of its algebraic data type, where the differentation is introducing a branch for every recursive component.[2]

The functions that will enable arbitrary navigation around the term are the following:

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

## The Environment

Using our zipper, we can now traverse the term and perform rewrites. However, while doing so we are going to encounter many subterms that reference a term by name, using a variable. Often, when we want to operate on such a term, we need to perform a lookup in the environment we are currently in.

As it happens, all the information required to perform a lookup is already captured within the context of our zipper, in the form of the `Abs'` and `LetR` terms. To resolve a reference to a term, we can thus walk the context recusively until we find the relevant binding.

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


## Faster Lookup

While walking term using the zipper functions can be done in _O(1)_, the lookup function presented in the previous chapter has time complexity _O(n)_, where _n_ is the number of nested terms above. This is a problem when the lookup is an often performed operation.

Using an efficient map, we can trade lookup complexity for traversal complexity (functions _up_, _down_, _left_ and _right_), both in relation to the number of bound variables.

    type Env t = Map String [(t, Term t)]
    
We choose this type to hold the information about our environment. It is a string-indexed map of term stacks. Intuitively, you could say we implement a map where you can peel off the current binding, and the previous binding will surface again.

    push :: String -> t -> Term t -> Env t -> Env t
    push key y t = Map.insertWith (++) key [(y, t)]

    pop :: String -> Env t -> Env t
    pop = Map.update tailMay

To have access to the environment from the traversal functions, we change the `Loc` data type to include a reference to `Env`.

    data Loc t = At { it :: Term t, ctx :: Ctx t, env :: Env t }

We modify the zipper functions slightly, to manipulate the environment while traversing the term.

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

Using a slightly different set of rules, it is trivial to adapt this to allow recursive let bindings.

Finally, the implementation of the new lookup function is trivial.

    lookup :: String -> Loc t -> Maybe (t, Term t)
    lookup key (At _ _ env) = Map.lookup key env >>= headMay

To reduce space usage, we could simplify the `Abs'` and `LetR` contexts to exclude redudant term information. However, in doing so, we would have to rely on unsafe operations internally (e.g. `fromJust`), since the link between items on the stack and inside the context would not be given.

---
    
[1]: Hinze, Ralf and Jeuring, Johan: "Weaving a Web"

[2]: McBride, Connor (2001): "The Derivative of a Regular Type is its Type of One-Hole Contexts"