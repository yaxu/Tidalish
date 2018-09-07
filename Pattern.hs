{-# LANGUAGE TypeSynonymInstances #-}

module Pattern where

import Data.Ratio
import Control.Applicative

type Time = Rational
type Span = (Time, Time)
type Part = (Span, Span) -- part, whole (first should fit inside the second)
type Query a = (Span -> [(Part, a)])
data Pattern a = Pattern {query :: Query a}

instance Functor Pattern where
  fmap f = Pattern . (fmap (fmap (fmap f))) . query

atom v = Pattern $ \(s,e) -> map (\(s',e') -> (constrain (s,e) (s',e'),v)) $ cycleSpansInSpan (s,e)
    where constrain (s,e) (s',e') = ((max s s', min e e'), (s',e'))

-- | Splits the given @Arc@ into a list of @Arc@s, at cycle boundaries.
spanCycles :: Span -> [Span]
spanCycles (s,e) | s >= e = []
                 | sam s == sam e = [(s,e)]
                 | otherwise = (s, nextSam s) : (spanCycles (nextSam s, e))

-- queries that span arcs. For example `arc p (0.5, 1.5)` would be
-- turned into two queries, `(0.5,1)` and `(1,1.5)`, and the results
-- combined. Being able to assume queries don't span cycles often
-- makes transformations easier to specify.
splitQueries :: Pattern a -> Pattern a
splitQueries p = Pattern $ \a -> concatMap (query p) $ spanCycles a

cat :: [Pattern a] -> Pattern a
cat [] = silence
cat ps = Pattern f
  where l = length ps
        f a = concatMap f' $ spanCycles a
        f' a = query (withResultTime (+offset) p) $  mapBoth (subtract offset) a
          where p = ps !! n
                r = (floor $ fst a) :: Int
                n = r `mod` l
                offset = (fromIntegral $ r - ((r - n) `div` l)) :: Time

fastcat :: [Pattern a] -> Pattern a
fastcat ps = fast (toTime $ length ps) $ cat ps

rotL :: Time -> Pattern a -> Pattern a
rotL t p = withResultTime (subtract t) $ withQueryTime (+ t) p

rotR :: Time -> Pattern a -> Pattern a
rotR t = rotL (0-t)

(<#) :: Pattern (a -> b) -> Pattern a -> Pattern b
pf <# px = Pattern $ \a -> concatMap applyX $ query pf a
  where 
        applyX event@((_,(ws,_)),f) =
          map (\(_,x) -> (fst event, f x)) $ query px (ws,ws)

(#>) :: Pattern (a -> b) -> Pattern a -> Pattern b
pf #> px = Pattern $ \a -> concatMap applyToF $ query px a
  where 
        applyToF event@((_,(ws,_)),x) =
          map (\(_,f) -> (fst event, f x)) $ query pf (ws,ws)

whole :: Span -> Part
whole s = (s,s)

toTime :: Integral a => a -> Rational
toTime = toRational

silence :: Pattern a
silence = Pattern $ const []


-- the 'sam' or start of cycle for the given time value
sam :: Time -> Time
sam = fromIntegral . floor

-- The end point of the current cycle (and starting point of the next cycle)
nextSam :: Time -> Time
nextSam = (1+) . sam

-- The span of the whole cycle that the given time value falls within
timeToCycleSpan :: Time -> Span
timeToCycleSpan t = (sam t, (sam t) + 1)

-- A list of cycle numbers which are included in the given span
cyclesInSpan :: Integral a => Span -> [a]
cyclesInSpan (s,e) | s > e = []
                   | s == e = [floor s]
                   | otherwise = [floor s .. (ceiling e)-1]

-- A list of spans of the whole cycles which are included in the given span
cycleSpansInSpan :: Span -> [Span]
cycleSpansInSpan = map (timeToCycleSpan . toTime) . cyclesInSpan

withResultSpan :: (Span -> Span) -> Pattern a -> Pattern a
withResultSpan f p = Pattern $ \a -> map (mapFst (mapBoth f)) $ query p a

withResultTime :: (Time -> Time) -> Pattern a -> Pattern a
withResultTime = withResultSpan . mapBoth

withQuerySpan :: (Span -> Span) -> Pattern a -> Pattern a
withQuerySpan f p = Pattern $ \a -> query p (f a)

withQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
withQueryTime = withQuerySpan . mapBoth

fast :: Time -> Pattern a -> Pattern a
fast r p | r == 0 = silence
         -- | r < 0 = rev $ fast (0-r) p
         | otherwise = withResultTime (/ r) $ withQueryTime (* r) p

slow :: Time -> Pattern a -> Pattern a
slow r p = fast (1/r) p

-- Utils
mapBoth :: (a -> a) -> (a,a) -> (a,a)
mapBoth f (a,b) = (f a, f b)

mapPartTimes :: (a -> a) -> ((a,a),(a,a)) -> ((a,a),(a,a))
mapPartTimes f part = mapBoth (mapBoth f) part

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

-- fast n p = 

{-
intersectSpan :: Span -> Span -> Maybe Span
intersectSpan (s, e) (s',e') | s'' < e'' = Just (s'', e'')
                             | otherwise = Nothing
  where s'' = max s s'
        e'' = min e e'
-}
