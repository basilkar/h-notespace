module Harmonics where

import Data.List
import Data.Maybe

type Cent = Double
type IntFreq = Integer
type Ival = (IntFreq, IntFreq)

harmonicsOf :: IntFreq -> [IntFreq]
harmonicsOf f = map (f*) [1..]

-- this finds all common frequencies among the first m harmonics of two fundamentals f and g
firstCommonHarmonics :: Int -> IntFreq -> IntFreq -> [IntFreq]
firstCommonHarmonics m f g = take m (harmonicsOf f) `intersect` take m (harmonicsOf g)

-- this considers the first k harmonics of f, and lists the number of the common frequencies that each of the harmonics has with f, looking at their respective first m harmonics
overlapWithHarmonics :: IntFreq -> Int -> Int -> [Int]
overlapWithHarmonics f m k = take k $ map (length . firstCommonHarmonics m f) (harmonicsOf f)
-- of importance is the case where m >= k, where the last overtones do get a chance to have common harmonics with the fundamental, eg
-- *Main> overlapWithHarmonics 55 1000 1000
-- [1000,500,333,250,200,166,142,125,111,100,90,83,76,71,66,62,58,55,52,50,47,45,43,41,40,38,37,35,34,33,32,31,30,29,28,27,27,26,25,25,24,23,23,22,22,21,21,20,20,20,19,19,18,18,18,17,17,17,16,16,16,16,15,15,15,15,14,14,14,14,14,13,13,13,13,13,12,12,12,12,12,12,12,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,9,9,9,9,9,9,9,9,9,9,9,8,8,8,8,8,8,8,8,8,8,8,8,8,8,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

octaveUp :: IntFreq -> IntFreq
octaveUp f = 2*f

octaveDown:: IntFreq -> IntFreq
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
    | num == den            = (1,1)
    | isWithinOctave ival   = ival
    | num < den             = octaveReduction (octaveUp num, den)
    | even num              = octaveReduction (octaveDown num, den)
    | otherwise             = octaveReduction (num, octaveUp den)
    where
        num = fst ival
        den = snd ival

harmonicInverse :: Ival -> Ival
harmonicInverse ival = octaveReduction (den, num)
    where
        num = fst ival
        den = snd ival

-- these return pure ratios
justHarmonicIntervals :: [Ival]
justHarmonicIntervals = map (\h -> octaveReduction (h, 1)) (harmonicsOf 1)

discreteJustHarmonicIntervals :: [Ival]
discreteJustHarmonicIntervals = nub justHarmonicIntervals

justHarmonicIntervalsDecimal :: [Float]
justHarmonicIntervalsDecimal = map (\(num, den) -> fromIntegral num / fromIntegral den) justHarmonicIntervals

discreteJustHarmonicIntervalsDecimal :: [Float]
discreteJustHarmonicIntervalsDecimal = nub justHarmonicIntervalsDecimal

equalTwelveToneTemperedIntervals :: [Float]
equalTwelveToneTemperedIntervals = map (\m -> 2 ** (m / 12)) [0..]

-- eg
-- *Main> sort $ take 12 discreteJustHarmonicIntervalsDecimal
-- [1.0,1.0625,1.125,1.1875,1.25,1.3125,1.375,1.4375,1.5,1.625,1.75,1.875]
-- *Main> take 12 equalTwelveToneTemperedIntervals
-- [1.0,1.0594631,1.122462,1.1892071,1.2599211,1.3348398,1.4142135,1.4983071,1.587401,1.6817929,1.7817974,1.8877486]

ratioToCents :: Floating a => (a, a) -> a
ratioToCents (num, den) = 1200 * logBase 2 (num / den)

-- PHYSIOANATOMICAL LIMITS
-- for the limen (smallest perceptible interval) Benson-2006 says 220 cents at 5dB and 31Hz; the higher the dB or Hz, the lower the limen
limen :: Cent
limen = 3
audibleMin :: IntFreq
audibleMin = 20
audibleMax :: IntFreq
audibleMax = 20000

isAboveLimen :: Ival -> Bool
isAboveLimen ival = ratioToCents (num, den) >= limen
    where
        num = fromInteger (fst ival)
        den = fromInteger (snd ival)

isBelowLimen :: Ival -> Bool
isBelowLimen ival = ratioToCents (num, den) < limen
    where
        num = fromInteger (fst ival)
        den = fromInteger (snd ival)

isAudible :: IntFreq -> Bool
isAudible f = audibleMin <= f && f <= audibleMax

audibles :: [IntFreq] -> [IntFreq]
audibles [] = []
audibles (f:fs)
    | isAudible f   = f : audibles fs
    | otherwise     = []

audibleHarmonicsOf :: IntFreq -> [IntFreq]
audibleHarmonicsOf = audibles . harmonicsOf

-- while these return the ratios times the fundamental
justHarmonicIntervalsOf :: IntFreq -> [Ival]
justHarmonicIntervalsOf f = map (\h -> octaveReduction (h, f)) (harmonicsOf f)

discreteJustHarmonicIntervalsOf :: IntFreq -> [Ival]
discreteJustHarmonicIntervalsOf = nub . justHarmonicIntervalsOf

discreteJustHarmonicIntervalsAboveLimen :: [Ival]
discreteJustHarmonicIntervalsAboveLimen = take numOfAboveLimen propers
    where
        numOfAboveLimen = fromJust (findIndex isBelowLimen propers)
        propers = tail discreteJustHarmonicIntervals

-- *Main> discreteJustHarmonicIntervalsAboveLimen 
-- [(1,1),(3,2),(5,4),(7,4),(9,8),(11,8),(13,8),(15,8),(17,16),(19,16),(21,16),(23,16),(25,16),(27,16),(29,16),(31,16),(33,32),(35,32),(37,32),(39,32),(41,32),(43,32),(45,32),(47,32),(49,32),(51,32),(53,32),(55,32),(57,32),(59,32),(61,32),(63,32),(65,64),(67,64),(69,64),(71,64),(73,64),(75,64),(77,64),(79,64),(81,64),(83,64),(85,64),(87,64),(89,64),(91,64),(93,64),(95,64),(97,64),(99,64),(101,64),(103,64),(105,64),(107,64),(109,64),(111,64),(113,64),(115,64),(117,64),(119,64),(121,64),(123,64),(125,64),(127,64),(129,128),(131,128),(133,128),(135,128),(137,128),(139,128),(141,128),(143,128),(145,128),(147,128),(149,128),(151,128),(153,128),(155,128),(157,128),(159,128),(161,128),(163,128),(165,128),(167,128),(169,128),(171,128),(173,128),(175,128),(177,128),(179,128),(181,128),(183,128),(185,128),(187,128),(189,128),(191,128),(193,128),(195,128),(197,128),(199,128),(201,128),(203,128),(205,128),(207,128),(209,128),(211,128),(213,128),(215,128),(217,128),(219,128),(221,128),(223,128),(225,128),(227,128),(229,128),(231,128),(233,128),(235,128),(237,128),(239,128),(241,128),(243,128),(245,128),(247,128),(249,128),(251,128),(253,128),(255,128),(257,256),(259,256),(261,256),(263,256),(265,256),(267,256),(269,256),(271,256),(273,256),(275,256),(277,256),(279,256),(281,256),(283,256),(285,256),(287,256),(289,256),(291,256),(293,256),(295,256),(297,256),(299,256),(301,256),(303,256),(305,256),(307,256),(309,256),(311,256),(313,256),(315,256),(317,256),(319,256),(321,256),(323,256),(325,256),(327,256),(329,256),(331,256),(333,256),(335,256),(337,256),(339,256),(341,256),(343,256),(345,256),(347,256),(349,256),(351,256),(353,256),(355,256),(357,256),(359,256),(361,256),(363,256),(365,256),(367,256),(369,256),(371,256),(373,256),(375,256),(377,256),(379,256),(381,256),(383,256),(385,256),(387,256),(389,256),(391,256),(393,256),(395,256),(397,256),(399,256),(401,256),(403,256),(405,256),(407,256),(409,256),(411,256),(413,256),(415,256),(417,256),(419,256),(421,256),(423,256),(425,256),(427,256),(429,256),(431,256),(433,256),(435,256),(437,256),(439,256),(441,256),(443,256),(445,256),(447,256),(449,256),(451,256),(453,256),(455,256),(457,256),(459,256),(461,256),(463,256),(465,256),(467,256),(469,256),(471,256),(473,256),(475,256),(477,256),(479,256),(481,256),(483,256),(485,256),(487,256),(489,256),(491,256),(493,256),(495,256),(497,256),(499,256),(501,256),(503,256),(505,256),(507,256),(509,256),(511,256),(513,512),(515,512),(517,512),(519,512),(521,512),(523,512),(525,512),(527,512),(529,512),(531,512),(533,512),(535,512),(537,512),(539,512),(541,512),(543,512),(545,512),(547,512),(549,512),(551,512),(553,512),(555,512),(557,512),(559,512),(561,512),(563,512),(565,512),(567,512),(569,512),(571,512),(573,512),(575,512),(577,512),(579,512),(581,512),(583,512),(585,512),(587,512),(589,512),(591,512),(593,512),(595,512),(597,512),(599,512),(601,512),(603,512),(605,512),(607,512),(609,512),(611,512),(613,512),(615,512),(617,512),(619,512),(621,512),(623,512),(625,512),(627,512),(629,512),(631,512),(633,512),(635,512),(637,512),(639,512),(641,512),(643,512),(645,512),(647,512),(649,512),(651,512),(653,512),(655,512),(657,512),(659,512),(661,512),(663,512),(665,512),(667,512),(669,512),(671,512),(673,512),(675,512),(677,512),(679,512),(681,512),(683,512),(685,512),(687,512),(689,512),(691,512),(693,512),(695,512),(697,512),(699,512),(701,512),(703,512),(705,512),(707,512),(709,512),(711,512),(713,512),(715,512),(717,512),(719,512),(721,512),(723,512),(725,512),(727,512),(729,512),(731,512),(733,512),(735,512),(737,512),(739,512),(741,512),(743,512),(745,512),(747,512),(749,512),(751,512),(753,512),(755,512),(757,512),(759,512),(761,512),(763,512),(765,512),(767,512),(769,512),(771,512),(773,512),(775,512),(777,512),(779,512),(781,512),(783,512),(785,512),(787,512),(789,512),(791,512),(793,512),(795,512),(797,512),(799,512),(801,512),(803,512),(805,512),(807,512),(809,512),(811,512),(813,512),(815,512),(817,512),(819,512),(821,512),(823,512),(825,512),(827,512),(829,512),(831,512),(833,512),(835,512),(837,512),(839,512),(841,512),(843,512),(845,512),(847,512),(849,512),(851,512),(853,512),(855,512),(857,512),(859,512),(861,512),(863,512),(865,512),(867,512),(869,512),(871,512),(873,512),(875,512),(877,512),(879,512),(881,512),(883,512),(885,512),(887,512),(889,512),(891,512),(893,512),(895,512),(897,512),(899,512),(901,512),(903,512),(905,512),(907,512),(909,512),(911,512),(913,512),(915,512),(917,512),(919,512),(921,512),(923,512),(925,512),(927,512),(929,512),(931,512),(933,512),(935,512),(937,512),(939,512),(941,512),(943,512),(945,512),(947,512),(949,512),(951,512),(953,512),(955,512),(957,512),(959,512),(961,512),(963,512),(965,512),(967,512),(969,512),(971,512),(973,512),(975,512),(977,512),(979,512),(981,512),(983,512),(985,512),(987,512),(989,512),(991,512),(993,512),(995,512),(997,512),(999,512),(1001,512),(1003,512),(1005,512),(1007,512),(1009,512),(1011,512),(1013,512),(1015,512),(1017,512),(1019,512),(1021,512),(1023,512)]
-- *Main> length discreteJustHarmonicIntervalsAboveLimen 
-- 512

discreteInverseJustHarmonicIntervalsAboveLimen :: [Ival]
discreteInverseJustHarmonicIntervalsAboveLimen = filter isAboveLimen (map harmonicInverse discreteJustHarmonicIntervalsAboveLimen)

discreteJustHarmonicIntervalsWithInversesAboveLimen :: [Ival]
discreteJustHarmonicIntervalsWithInversesAboveLimen = sort $ nub $ directs ++ inverses
    where
        directs = discreteJustHarmonicIntervalsAboveLimen
        inverses = discreteInverseJustHarmonicIntervalsAboveLimen