# h-notespace

Sample features:

1. input a set of notes, output fitting scales {haskell-prototyping.hs: improScale}  
2. input a scale, output its chords {haskell-prototyping.hs: scalechords}  
3. recognize known chords and scales {haskell-prototyping.hs: ictRecognizer}  
4. print all seventh chords within a given scale  
5. input a melody, output a harmonization  
6. input keys X, Y and an n, output n keys on a tonnetz/circle-of-fifths/... geodesic of X & Y  
7. input a chord progression, output its negative (in the sense of Ernst Levy)  
8. run fretboard quizzes {haskell-prototyping.hs: fretboardQuizOneStandard, fretboardQuizTwoStandard}  

## Compilation

Working in `stack`, you should just  
`~/h-notespace$ stack build`  
and then  
`~/h-notespace$ stack exec h-notespace-exe`

## Contents of the old file `haskell-prototyping.hs`

0. Preamble and stolen stuff  
1. An enumeration type for notes  
1.1. Application: Impro suggester  
1.2. Application: Scale chords  
1.3. Application: Interval-chord-scale recognizer  
1.4. Application: The seventh chords of a scale  
1.5. Application: A literate suggester  
2. An inductive type for chord constructions  
3. About rhythm  
4. Tymoczko chord change spaces  
4.1. T-Closeness  
5. Concerning the guitar  
5.1. Application: Fretboard quizes  