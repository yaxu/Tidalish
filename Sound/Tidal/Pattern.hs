module Sound.Tidal.Pattern where

import Data.Ratio
import Control.Applicative
import Data.Maybe (mapMaybe)

import Sound.Tidal.Utils

------------------------------------------------------------------------
-- Types

-- | Time is rational
type Time = Rational

-- | A time span (start and end)
type Span = (Time, Time)

-- | The first timespan (the part) should be equal to or fit inside the
-- second one (the whole that it's a part of)
type Part = (Span, Span)

-- | An event is a value that's active during a timespan
type Event a = (Part, a)

-- | A function that represents events taking place over time
type Query a = (Span -> [Event a])

-- | A datatype that's basically a query. At least for now.
data Pattern a = Pattern {query :: Query a}

------------------------------------------------------------------------
-- Instances

-- | Repeat the given value once per cycle, forever
atom v = Pattern $ \(s,e) -> map (\(s',e') -> (constrain (s,e) (s',e'),v)) $ cycleSpansInSpan (s,e)
    where constrain (s,e) (s',e') = ((max s s', min e e'), (s',e'))

instance Functor Pattern where
  -- | apply a function to all the values in a pattern
  fmap f = Pattern . (fmap (fmap (fmap f))) . query

instance Applicative Pattern where
  pure = atom
  -- | Takes a pattern of values and applies them to a pattern of
  -- functions, where the 'smallest' events give the resulting
  -- structure.
  -- Step 1: Matches the *onsets* of the events on the right that fit
  --   inside the whole *spans* of the events on the left. It takes
  --   the structure from the right, but filters out events on the
  --   right that are longer than their matched events on the left. Applies
  --   the values to their functions.
  -- Step 2: Vice-versa (i.e., doing the same as above, but
  --   with left and right swapped)
  -- Step 3: Append the list of events from step 2, to those from step 1
  pf <*> px = Pattern $ \a -> (concatMap applyX $ query pf a) ++ (concatMap applyToF $ query px a)
    where
      applyX event@((_,(ws,_)),f) =
        map (\(_,x) -> (fst event, f x)) $ filter (eventLE event) $ query px (ws,ws)
      applyToF event@((_,(ws,_)),x) =
        map (\(_,f) -> (fst event, f x)) $ filter (eventL event) $ query pf (ws,ws)
      eventL :: Event a -> Event b -> Bool
      eventL e e' = (delta $ eventWhole e) < (delta $ eventWhole e')
      eventLE :: Event a -> Event b -> Bool
      eventLE e e' = (delta $ eventWhole e) <= (delta $ eventWhole e')

-- | Like <*>, but the structure comes from the left
(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
pf <* px = Pattern $ \a -> concatMap applyX $ query pf a
  where 
        applyX event@((_,(ws,_)),f) =
          map (\(_,x) -> (fst event, f x)) $ query px (ws,ws)

-- | Like <*>, but the structure comes from the right
(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
pf *> px = Pattern $ \a -> concatMap applyToF $ query px a
  where 
        applyToF event@((_,(ws,_)),x) =
          map (\(_,f) -> (fst event, f x)) $ query pf (ws,ws)

instance Monad Pattern where
  return = atom
  p >>= f = unwrap (f <$> p)

-- | Turns a pattern of patterns into a single pattern, like this:
-- 1/ For query @span@, get the events from the outer pattern
-- 2/ For each event, get the 'whole' timespan and internal pattern,
-- querying it with the same @span@
-- 3/ Munge each of the resulting inner events using the 'whole'
-- timespan of the outer event, as follows
--  a/ Get the new part as the intersection of the outer whole and inner part
--  b/ Keep the new whole as the inner whole
-- 4/ concatenate all the inner events together

unwrap :: Pattern (Pattern a) -> Pattern a
unwrap p = Pattern $ \span -> concatMap (\((_, whole), p') -> mapMaybe (munge whole) $ query p' span) (query p span)
  where munge outerWhole ((innerPart,innerWhole),v) =
          do part' <- subSpan outerWhole innerPart
             return ((part', innerWhole),v)


------------------------------------------------------------------------
-- Internal functions

-- | Get the timespan of an event's 'whole'
eventWhole :: Event a -> Span
eventWhole = snd . fst

-- | Get the timespan of an event's 'part'
eventPart :: Event a -> Span
eventPart = fst . fst

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

-- | The 'sam' (start of cycle) for the given time value
sam :: Time -> Time
sam = fromIntegral . floor

-- | Turns a number into a (rational) time value. An alias for @toRational@.
toTime :: Real a => a -> Rational
toTime = toRational

-- | The end point of the current cycle (and starting point of the next cycle)
nextSam :: Time -> Time
nextSam = (1+) . sam

-- | @subSpan i j@ is the timespan that is the intersection of @i@ and @j@.
subSpan :: Span -> Span -> Maybe Span
subSpan (s, e) (s',e') | s'' < e'' = Just (s'', e'')
                      | otherwise = Nothing
  where s'' = max s s'
        e'' = min e e'

-- | The span of the whole cycle that the given time value falls within
timeToCycleSpan :: Time -> Span
timeToCycleSpan t = (sam t, (sam t) + 1)

-- | A list of cycle numbers which are included in the given span
cyclesInSpan :: Integral a => Span -> [a]
cyclesInSpan (s,e) | s > e = []
                   | s == e = [floor s]
                   | otherwise = [floor s .. (ceiling e)-1]

-- | A list of spans of the whole cycles which are included in the given span
cycleSpansInSpan :: Span -> [Span]
cycleSpansInSpan = map (timeToCycleSpan . toTime) . cyclesInSpan

-- | Apply a function to the timespans (both whole and parts) of the result
withResultSpan :: (Span -> Span) -> Pattern a -> Pattern a
withResultSpan f p = Pattern $ \a -> map (mapFst (mapBoth f)) $ query p a

-- | Apply a function to the time (both start and end of the timespans
-- of both whole and parts) of the result
withResultTime :: (Time -> Time) -> Pattern a -> Pattern a
withResultTime = withResultSpan . mapBoth

-- | Apply a function to the timespan of the query
withQuerySpan :: (Span -> Span) -> Pattern a -> Pattern a
withQuerySpan f p = Pattern $ \a -> query p (f a)

-- | Apply a function to the time (both start and end) of the query
withQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
withQueryTime = withQuerySpan . mapBoth

------------------------------------------------------------------------
-- UI

-- | An empty pattern
silence :: Pattern a
silence = Pattern $ const []

-- | Interlace the given patterns, playing the first cycle from each
-- in turn, then the second cycle from each, and so on.
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

slowCat = cat
slowcat = slowCat

append a b = cat [a,b]

slowAppend = append
slowappend = append

fastAppend a b = fast 2 $ append a b
fastappend = fastAppend

-- | The same as @cat@, but speeds up the result by the number of
-- patterns there are, so the cycles from each are squashed to fit a
-- single cycle.
fastCat :: [Pattern a] -> Pattern a
fastCat ps = fast (toTime $ length ps) $ cat ps

fastcat = fastCat

fromValues :: [a] -> Pattern a
fromValues = fastCat . map atom

listToPat = fromValues

-- | @fromMaybes@ is similar to @fromValues@, but allows values to
-- be optional using the @Maybe@ type, so that @Nothing@ results in
-- gaps in the pattern.
fromMaybes :: [Maybe a] -> Pattern a
fromMaybes = fastcat . map f
  where f Nothing = silence
        f (Just x) = atom x

-- | Shifts a pattern back in time by the given amount, expressed in cycles
rotL :: Time -> Pattern a -> Pattern a
rotL t p = withResultTime (subtract t) $ withQueryTime (+ t) p

-- | Infix alias for @rotL@
(<~) = rotL

-- | Shifts a pattern forward in time by the given amount, expressed in cycles
rotR :: Time -> Pattern a -> Pattern a
rotR t = rotL (0-t)

-- | Infix alias for @rotR@
(~>) = rotR

-- | Speed up a pattern by the given factor
fast :: Time -> Pattern a -> Pattern a
fast r p | r == 0 = silence
         -- | r < 0 = rev $ fast (0-r) p
         | otherwise = withResultTime (/ r) $ withQueryTime (* r) p

-- | Slow down a pattern by the given factor
slow :: Time -> Pattern a -> Pattern a
slow r p = fast (1/r) p

run :: (Enum a, Num a) => a -> Pattern a
run n = fromValues [0 .. n-1]

scan :: (Enum a, Num a) => a -> Pattern a
scan n = cat $ map run [1 .. n]
