module MyLib where

data Formula = Var String
          | Formula :&: Formula
          | Formula :|: Formula
          | Formula :->: Formula
          | Formula :<->: Formula
          | Not Formula
         deriving (Eq, Show)


toNNF :: Formula -> Formula

toNNF (Var a)       = Var a
toNNF (Not (Var a)) = Not (Var a)

toNNF (Not (Not f)) = toNNF f

toNNF (f1 :&: f2)       = toNNF f1 :&: toNNF f2
toNNF (Not (f1 :&: f2)) = toNNF (Not f1) :|: toNNF (Not f2)

toNNF (f1 :|: f2)       = toNNF f1 :|: toNNF f2
toNNF (Not (f1 :|: f2)) = toNNF (Not f1) :&: toNNF (Not f2)

toNNF (f1 :->: f2)       = toNNF (Not f1) :|: toNNF f2
toNNF (Not (f1 :->: f2)) = toNNF f1 :&: toNNF (Not f2)

toNNF (f1 :<->: f2)       = (toNNF (Not f1) :|: toNNF f2) :&: (toNNF f1 :|: toNNF (Not f2))
toNNF (Not (f1 :<->: f2)) = (toNNF (Not f1) :&: toNNF f2) :|: (toNNF f1 :&: toNNF (Not f2))


toCNF :: Formula -> Formula
toCNF = toCNF' . toNNF

toCNF' :: Formula -> Formula
toCNF' (f1 :&: f2) = toCNF f1 :&: toCNF f2
toCNF' (f1 :|: f2) = disjDistributivity (toCNF' f1) (toCNF' f2)
toCNF' f = f

disjDistributivity :: Formula -> Formula -> Formula
disjDistributivity f1 (f2 :&: f3) = (f1 :|: f2) :&: (f1 :|: f3)
disjDistributivity (f1 :&: f2) f3 = (f1 :|: f3) :&: (f2 :|: f3)
disjDistributivity f1 f2 = f1 :|: f2


toDNF = toDNF' . toNNF

toDNF' :: Formula -> Formula
toDNF' (f1 :|: f2) = toDNF f1 :|: toDNF f2
toDNF' (f1 :&: f2) = conjDistributivity (toDNF' f1) (toDNF' f2)
toDNF' f = f

conjDistributivity :: Formula -> Formula -> Formula
conjDistributivity f1 (f2 :|: f3) = (f1 :&: f2) :|: (f1 :&: f3)
conjDistributivity (f1 :|: f2) f3 = (f1 :&: f3) :|: (f2 :&: f3)
conjDistributivity f1 f2 = f1 :&: f2






