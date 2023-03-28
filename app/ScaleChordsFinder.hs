module ScaleChordsFinder where

import Note

{------------------------------------}
{- 1.2 -- APPLICATION: SCALE CHORDS -}
{------------------------------------}

{-

Produce the chords of a given scale, according to a given signature.

INPUT a scale and a signature
OUTPUT a list of lists of notes from the given scale that fit the given signature

-}

scalechords :: [Note] -> [Int] -> [[Note]]
scalechords [] _ = []
scalechords _ [] = []
scalechords sc sig = [ notesBySignature (head sc) c | c <- memoArray, all (`elem` scsig) c]
    where
        scsig = signatureByNotes sc
        memoArray = map (\ m -> map (\ n -> (n + m) `mod` 12) sig) scsig

{- EXAMPLES -}

{-
c_major_scale = scaleM C -- alternatively, c_major_scale = notesBySignature C sigScaleM


The following find all major, minor, augmented, and diminished triads to be found in the key of C major.

> scalechords c_major_scale sigTriadM
[[C,E,G],[F,A,C],[G,B,D]]

> scalechords c_major_scale sigTriadm
[[D,F,A],[E,G,B],[A,C,E]]

> scalechords c_major_scale sigTriada
[]

> scalechords c_major_scale sigTriadd
[[B,D,F]]

We can find all (traditional) triads to be found in the key of C major

> map (\ sigs -> scalechords c_major_scale sigs) [sigTriadM, sigTriadm, sigTriada, sigTriadd]
[[[C,E,G],[F,A,C],[G,B,D]],[[D,F,A],[E,G,B],[A,C,E]],[],[[B,D,F]]]

or in the scale of E minor pentatonic

> map (\ sigs -> scalechords (notesBySignature E sigScalepm) sigs) [sigTriadM, sigTriadm, sigTriada, sigTriadd]
[[[G,B,D]],[[E,G,B]],[],[]]

and in the same fashion we can find all (traditional) seventh chords in these keys

> map (\ sigs -> scalechords c_major_scale sigs) [sig7ChordMM, sig7ChordMm, sig7Chordmm, sig7ChordmM, sig7Chorddm, sig7Chordam]

The same example for, say, the E harmonic major:


e_harmonic_major = scaleMh E


> map (\ sigs -> scalechords e_harmonic_major sigs) [sig7ChordMM, sig7ChordMm, sig7Chordmm, sig7ChordmM, sig7Chorddm, sig7Chordam]
[[[E,Gs,B,Ds]],[[Gs,C,Ds,Fs],[B,Ds,Fs,A]],[[A,C,E,Gs]],[[Gs,B,Ds,Fs]],[[Fs,A,C,E]],[[Gs,C,E,Fs]]]

Similarly, we can use the function scalechords to find all perfect fourth intervals, say, of A minor pentatonic.mode 6 scalemm G

> map (\ sigs -> scalechords (notesBySignature E sigScalepm) sigs) [ sigInterval4p ]
[[[E,A],[A,D],[B,E],[D,G]]]

and, to contrast, all perfect fourth intervals of C major:

> map (\ sigs -> scalechords c_major_scale sigs) [ sigInterval4p ]
[[[C,F],[D,G],[E,A],[G,C],[A,D],[B,E]]]

Finally, all tritones in A melodic minor.

> map (\ sigs -> scalechords (notesBySignature A sigScalemm) sigs) [ sigInterval4a ]
[[[C,Fs],[D,Gs],[Fs,C],[Gs,D]]]

Also, we can find existing instances of a scale or mode within a given scale. For example, this is how we find which notes of E major key constitute its locrian mode (the seventh mode of a major scale).

> scalechords (scaleM E) (signatureByNotes [B, C, D, E, F, G, A])

-}