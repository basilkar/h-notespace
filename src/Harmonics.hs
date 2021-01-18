import Data.List

type Cent = Float
type Freq = Integer
type Ival = (Freq, Freq)

harmonicsOf :: Freq -> [Freq]
harmonicsOf f = map (f*) [1..]

-- this finds all common frequencies among the first m harmonics of two fundamentals f and g
firstCommonHarmonics :: Int -> Freq -> Freq -> [Freq]
firstCommonHarmonics m f g = take m (harmonicsOf f) `intersect` take m (harmonicsOf g)

-- this considers the first k harmonics of f, and lists the number of the common frequencies that each of the harmonics has with f, looking at their respective first m harmonics
overlapWithHarmonics :: Freq -> Int -> Int -> [Int]
overlapWithHarmonics f m k = take k $ map (length . firstCommonHarmonics m f) (harmonicsOf f)
-- of importance is the case where m >= k, where the last overtones do get a chance to have common harmonics with the fundamental, eg
-- *Main> overlapWithHarmonics 55 1000 1000
-- [1000,500,333,250,200,166,142,125,111,100,90,83,76,71,66,62,58,55,52,50,47,45,43,41,40,38,37,35,34,33,32,31,30,29,28,27,27,26,25,25,24,23,23,22,22,21,21,20,20,20,19,19,18,18,18,17,17,17,16,16,16,16,15,15,15,15,14,14,14,14,14,13,13,13,13,13,12,12,12,12,12,12,12,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,9,9,9,9,9,9,9,9,9,9,9,8,8,8,8,8,8,8,8,8,8,8,8,8,8,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]


-- TODO
-- isHarmonicOf :: Freq -> Freq -> Bool
-- isHarmonicOf f g
--     | f < g = False
--     | otherwise

-- this is to be read as "the first m missing harmonics of f in g"
-- firstMissingHarmonics :: Int -> Freq -> Freq -> [Freq]
-- firstMissingHarmonics m f g = take m [c | c <- harmonicsOf f, not $ c `elem` (harmonicsOf g)]

octaveUp :: Freq -> Freq
octaveUp f = 2*f

octaveDown:: Freq -> Freq
octaveDown f
    | odd f     = error "Harmonics: non-integer frequencies are not supported."
    | otherwise = f `div` 2

isWithinOctave :: Ival -> Bool
isWithinOctave ival = (num >= den) && (num < 2*den)
    where
        num = fst ival
        den = snd ival

octaveReduction :: Ival -> Ival
octaveReduction ival
    | isWithinOctave ival = ival
    | num < den         = octaveReduction (octaveUp num, den)
    | even num       = octaveReduction (octaveDown num, den)
    | otherwise         = octaveReduction (num, octaveUp den)
    where
        num = fst ival
        den = snd ival

harmonicInverse :: Ival -> Ival
harmonicInverse ival = octaveReduction (den, num)
    where
        num = fst ival
        den = snd ival

-- these return pure ratios
justIntervals :: [Ival]
justIntervals = map (\h -> octaveReduction (h, 1)) (harmonicsOf 1)

discreteJustIntervals :: [Ival]
discreteJustIntervals = nub justIntervals

justIntervalsDecimal :: [Float]
justIntervalsDecimal = map (\(num, den) -> fromIntegral num / fromIntegral den) justIntervals

discreteJustIntervalsDecimal :: [Float]
discreteJustIntervalsDecimal = nub justIntervalsDecimal

equalTwelveToneTemperedIntervals :: [Float]
equalTwelveToneTemperedIntervals = map (\m -> 2 ** (m / 12)) [0..]

-- eg
-- *Main> sort $ take 12 discreteJustIntervalsDecimal
-- [1.0,1.0625,1.125,1.1875,1.25,1.3125,1.375,1.4375,1.5,1.625,1.75,1.875]
-- *Main> take 12 equalTwelveToneTemperedIntervals
-- [1.0,1.0594631,1.122462,1.1892071,1.2599211,1.3348398,1.4142135,1.4983071,1.587401,1.6817929,1.7817974,1.8877486]

-- TODO
ratioToCents :: Floating a => (a, a) -> a
ratioToCents (num, den) = 1200 * logBase 2 (num / den)

-- PHYSIOANATOMICAL LIMITS
-- for the limen (smallest perceptible interval) Benson-2006 says 220 cents at 5dB and 31Hz; the higher the dB or Hz, the lower the limen
limen :: Double
limen = 220
audibleMin :: Freq
audibleMin = 20
audibleMax :: Freq
audibleMax = 20000

isAboveLimen :: Ival -> Bool
isAboveLimen ival = ratioToCents (num, den) >= limen
    where
        num = fromInteger (fst ival)
        den = fromInteger (snd ival)

isAudible :: Freq -> Bool
isAudible f = audibleMin <= f && f <= audibleMax

audibles :: [Freq] -> [Freq]
audibles [] = []
audibles (f:fs)
    | isAudible f = f : audibles fs
    | otherwise = []

audibleHarmonicsOf :: Freq -> [Freq]
audibleHarmonicsOf = audibles . harmonicsOf

-- while these return the ratios times the fundamental
justIntervalsOf :: Freq -> [Ival]
justIntervalsOf f = map (\h -> octaveReduction (h, f)) (harmonicsOf f)

discreteJustIntervalsOf :: Freq -> [Ival]
discreteJustIntervalsOf = nub . justIntervalsOf
