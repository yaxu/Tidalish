{-# LANGUAGE TypeSynonymInstances #-}

module Sound.Tidal.Pattern where

import Data.Ratio
import Control.Applicative

import Sound.Tidal.Utils

type Time = Rational
type Span = (Time, Time)
type Part = (Span, Span) -- part, whole (first should fit inside the second)
type Event a = (Part, a)
type Query a = (Span -> [Event a])
data Pattern a = Pattern {query :: Query a}

eventWhole :: Event a -> Span
eventWhole = snd . fst

eventPart :: Event a -> Span
eventPart = fst . fst

delta (a,b) = b-a

instance Functor Pattern where
  fmap f = Pattern . (fmap (fmap (fmap f))) . query

instance Applicative Pattern where
  pure = atom
  pf <*> px = Pattern $ \a -> (concatMap applyX $ query pf a) ++ (concatMap applyToF $ query px a)
    where
      applyX event@((_,(ws,_)),f) =
        map (\(_,x) -> (fst event, f x)) $ filter (eventLE event) $ query px (ws,ws)
      applyToF event@((_,(ws,_)),x) =
        map (\(_,f) -> (fst event, f x)) $ filter (eventL event) $ query pf (ws,ws)

(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
pf <* px = Pattern $ \a -> concatMap applyX $ query pf a
  where 
        applyX event@((_,(ws,_)),f) =
          map (\(_,x) -> (fst event, f x)) $ query px (ws,ws)

(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
pf *> px = Pattern $ \a -> concatMap applyToF $ query px a
  where 
        applyToF event@((_,(ws,_)),x) =
          map (\(_,f) -> (fst event, f x)) $ query pf (ws,ws)

eventL :: Event a -> Event b -> Bool
eventL e e' = (delta $ eventWhole e) < (delta $ eventWhole e')

eventLE :: Event a -> Event b -> Bool
eventLE e e' = (delta $ eventWhole e) <= (delta $ eventWhole e')


-- | Repeat the given value once per cycle, forever
atom v = Pattern $ \(s,e) -> map (\(s',e') -> (constrain (s,e) (s',e'),v)) $ cycleSpansInSpan (s,e)
    where constrain (s,e) (s',e') = ((max s s', min e e'), (s',e'))

-- | Splits the given @Span@ into a list of @Span@s, at cycle boundaries.
spanCycles :: Span -> [Span]
spanCycles (s,e) | s >= e = []
                 | sam s == sam e = [(s,e)]
                 | otherwise = (s, nextSam s) : (spanCycles (nextSam s, e))

-- | Splits queries that span arcs. For example `query p (0.5, 1.5)` would be
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
