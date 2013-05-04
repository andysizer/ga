{-# OPTIONS_GHC -XRankNTypes -XGADTs #-}

type Matcher = forall a. (Eq a) => a -> a -> Bool

defaultMatchFn :: Matcher
defaultMatchFn x y = x == y

data Match a b c d =   None
                     | Simple { index1 :: a
                                   , item1  :: b
                                   , index2 :: c
                                   , item2  :: d
                                   , next   :: Match a b c d
                       } 
                     | Choice { choice1 :: Match a b c d
                                   , choice2 :: Match a b c d
                       } 
                     deriving (Show)

align s1 s2 = align' s1 s2 defaultMatchFn

align' s1 s2 f = align'' 0 s1 0 s2 f

align'' _ [] _ _ _ = None
align'' _ _ _ [] _ = None
align'' i1 (e1:s1) i2 (e2:s2) matcher
    | matcher e1 e2  = Simple i1 e1 i2 e2 (align'' (i1 + 1) s1 (i2 + 1) s2 matcher)
    | otherwise    = pickMatch i1 s1 r1 i2 s2 r2 matcher
        where r1 = findMatch e1 s2 1 matcher
              r2 = findMatch e2 s1 1 matcher

data MatchResult a b c d where 
     NotFound :: MatchResult a b c d
     MkMatchResult :: (Ord a) => a -> b -> c -> d -> MatchResult a b c d

findMatch _ [] _ _        = NotFound
findMatch e (h:s) r match 
    | match e h      = MkMatchResult r e h s
    | otherwise      = findMatch e s (r + 1) match

pickMatch _ _ NotFound _ _ NotFound _                        = None
pickMatch i1 s (MkMatchResult d i m r) i2 _ NotFound matcher = Simple i1 i i2' m nextMatch
    where i2'       = i2 + d
          nextMatch = align'' (i1+1) s (i2'+1) r matcher
pickMatch i1 _ NotFound i2 s (MkMatchResult d i m r) matcher = Simple i1' m i2 i nextMatch
    where i1'       = i1 + d
          nextMatch = align'' (i1'+1) r (i2+1) s matcher
pickMatch i1 s1 (MkMatchResult d1 e1 m1 r1) i2 s2 (MkMatchResult d2 e2 m2 r2) matcher 
    | d1 < d2 = 
        let i2'       = i2 + d1
            nextMatch = align'' (i1+1) s1 (i2'+1) r1 matcher
        in Simple i1 e1 i2' m1 nextMatch
    | d2 < d1 = 
        let i1'       = i1 + d2
            nextMatch = align'' (i1'+1) r2 (i2+1) s2 matcher
        in Simple i1' e2 i2 m2 nextMatch
    | otherwise = 
        let i1'       = i1 + d2
            i2'       = i2 + d1
            nextMatch1 = align'' (i1+1) s1 (i2'+1) r1 matcher
            nextMatch2 = align'' (i1'+1) r2 (i2+1) s2 matcher
            match1 = Simple i1 e1 i2' m1 nextMatch1
            match2 = Simple i1' e2 i2 m2 nextMatch2
        in Choice match1 match2
