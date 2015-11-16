{-# LANGUAGE ViewPatterns, PatternSynonyms, ScopedTypeVariables #-}

module Ast where

data AstView a v = App a a
                 | Var v
                 | Fun v a

class Ast a where
  view :: a v n -> AstView (a v n) v
  -- annot :: a v n -> n
  app  :: n -> a v n -> a v n
  fun  :: n -> v -> a v n -> a v n
  var  :: n -> v -> a v n

-- pattern Lait v e b <- App (view -> Fun v b) e


--lait :: Ast a => n -> v -> a v n -> a v n -> a v n
--lait n v e b = app n (fun n v b) e

-- for fun
depth :: Ast a => a v n -> Int
--depth (view -> Lait v e b) = (max (depth e) (depth b)) + 1
--depth (view -> (App (view -> (Fun _ b)) e)) = (max (depth e) (depth b)) + 1
--depth (view -> (App (view -> x) y)) = 1
depth (view -> (App _ _)) = 1
depth (view -> (Fun _ _)) = 1
depth (view -> (Var _)) = 1

depth3 (x | (App _ _) <- x)

depth2 :: Ast a => a v n -> Int
depth2 x = case view x of
    App _ _ -> 1
    Fun _ _ -> 1
    Var _ -> 1
