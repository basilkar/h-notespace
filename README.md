# h-notespace

_h-notespace_ is a collection of music-relevant calculations implemented in haskell.

The project is work in progress, in that
- it currently just offers a command-line interface,
- some of the features are still musically inacurrate,
- not all intended features are implemented yet,
- the code iself reeks of prototyping -- a refactoring is in order.


## Compilation via stack

- Install the haskell project tool [stack](https://docs.haskellstack.org/en/stable/).
- Build _h-notespace_ by 
    ```shell
    ~/h-notespace$ stack build
    ```  
- Run _h-notespace_ by  
    ```shell
    ~/h-notespace$ stack exec h-notespace-exe
    ```  

## Usage and supported calculations

Once you run h-notespace you get the intro message
```
This is notespace, a music calculator.
In the applications below you enter notes as, e.g., Fs, F#, fs, f#, Gb or gb.
Choose application (or 0 to exit):
1 impro suggester
2 namer
3 scale chord finder
4 known scale chord finder
5 player
6 fretboard quizzer
```

### Impro suggester (work in progress)

Input a set of notes and a root and get a list of suggestions for improvisations.

### Namer

Input a set of notes and a root and get a name for them.

Sample run:
```
RECOGNIZER: What notes do you want to name?
 Enter a note to continue or just hit enter to finish:
a
 Enter a note to continue or just hit enter to finish:
c#
 Enter a note to continue or just hit enter to finish:
e
 Enter a note to continue or just hit enter to finish:

RECOGNIZER: You chose the notes:
[A,Cs,E]
RECOGNIZER: What would be the root?
a
RECOGNIZER: "TRIAD: A major"
```

### Scale chord finder

Input a set of notes and an example of a chord (not necessarilly consisting of the notes you've provided), and get all chords in the set you've provided that are the same type of chord as the example.

Sample run:
```
RECOGNIZER: In what notes do you want to search for chords?
 Enter a note to continue or just hit enter to finish:
e
 Enter a note to continue or just hit enter to finish:
f#
 Enter a note to continue or just hit enter to finish:
g
 Enter a note to continue or just hit enter to finish:
a
 Enter a note to continue or just hit enter to finish:
b
 Enter a note to continue or just hit enter to finish:
c#
 Enter a note to continue or just hit enter to finish:
d
 Enter a note to continue or just hit enter to finish:

RECOGNIZER: You chose the notes:
[E,Fs,G,A,B,Cs,D]
RECOGNIZER: What is an example of a chord whose kind you are looking for in these notes?
 Enter a note to continue or just hit enter to finish:
c
 Enter a note to continue or just hit enter to finish:
e
 Enter a note to continue or just hit enter to finish:
g
 Enter a note to continue or just hit enter to finish:

RECOGNIZER: You chose to look for chords like:
[C,E,G]
RECOGNIZER: Here are chords like this in the notes you've provided:
[G,B,D]
[A,Cs,E]
[D,Fs,A]
```

### Known scale chord finder

Input a set of notes and get all chords out of these notes of a known type.

Sample run:
```
RECOGNIZER: In what notes do you want to search for known chords?
 Enter a note to continue or just hit enter to finish:
e
 Enter a note to continue or just hit enter to finish:
f#
 Enter a note to continue or just hit enter to finish:
g
 Enter a note to continue or just hit enter to finish:
a
 Enter a note to continue or just hit enter to finish:
b
 Enter a note to continue or just hit enter to finish:
c#
 Enter a note to continue or just hit enter to finish:
d
 Enter a note to continue or just hit enter to finish:

RECOGNIZER: You chose the notes:
[E,Fs,G,A,B,Cs,D]
RECOGNIZER: Here are known chords in the notes you've provided:
"TRIAD: G major"
"TRIAD: A major"
"TRIAD: D major"
"TRIAD: E minor"
"TRIAD: Fs minor"
"TRIAD: B minor"
"TRIAD: Cs diminished"
"TETRAD: G major seventh"
"TETRAD: D major seventh"
"TETRAD: A seventh"
"TETRAD: E minor seventh"
"TETRAD: Fs minor seventh"
"TETRAD: B minor seventh"
"TETRAD: Cs half-diminished, aka minor seventh b5"
```

### Player

Input a series of notes in "nod triples", that is, in scientific pitch notation with a duration in beats (see sample run below for examples), and get the melody played back.

Based on an [idea of tsoding](https://github.com/tsoding/haskell-music/) the implementation currently uses `ffplay` of the [ffmpeg](https://ffmpeg.org/) framework, so you have to have this installed in your system for the player to work. The player saves the melody in a temporary `bin` file, which is deleted right after playback.

Sample run:
```
PLAYER: Input a melody that you want to play in note-octave-duration triples (the duration given in number of beats):
 Enter a nod triple to continue or just hit enter to finish:
eb-4-2
 Enter a nod triple to continue or just hit enter to finish:
c-4-1
 Enter a nod triple to continue or just hit enter to finish:
f-4-1
 Enter a nod triple to continue or just hit enter to finish:
eb-4-2
 Enter a nod triple to continue or just hit enter to finish:
c-4-2
 Enter a nod triple to continue or just hit enter to finish:
eb-4-1
 Enter a nod triple to continue or just hit enter to finish:
eb-4-1
 Enter a nod triple to continue or just hit enter to finish:
c-4-1
 Enter a nod triple to continue or just hit enter to finish:
f-4-1
 Enter a nod triple to continue or just hit enter to finish:
eb-4-2
 Enter a nod triple to continue or just hit enter to finish:
c-4-2
 Enter a nod triple to continue or just hit enter to finish:

ffplay version 3.4.11-0ubuntu0.1 Copyright (c) 2003-2022 the FFmpeg developers
  built with gcc 7 (Ubuntu 7.5.0-3ubuntu1~18.04)
  configuration: --prefix=/usr --extra-version=0ubuntu0.1 --toolchain=hardened --libdir=/usr/lib/x86_64-linux-gnu --incdir=/usr/include/x86_64-linux-gnu --enable-gpl --disable-stripping --enable-avresample --enable-avisynth --enable-gnutls --enable-ladspa --enable-libass --enable-libbluray --enable-libbs2b --enable-libcaca --enable-libcdio --enable-libflite --enable-libfontconfig --enable-libfreetype --enable-libfribidi --enable-libgme --enable-libgsm --enable-libmp3lame --enable-libmysofa --enable-libopenjpeg --enable-libopenmpt --enable-libopus --enable-libpulse --enable-librubberband --enable-librsvg --enable-libshine --enable-libsnappy --enable-libsoxr --enable-libspeex --enable-libssh --enable-libtheora --enable-libtwolame --enable-libvorbis --enable-libvpx --enable-libwavpack --enable-libwebp --enable-libx265 --enable-libxml2 --enable-libxvid --enable-libzmq --enable-libzvbi --enable-omx --enable-openal --enable-opengl --enable-sdl2 --enable-libdc1394 --enable-libdrm --enable-libiec61883 --enable-chromaprint --enable-frei0r --enable-libopencv --enable-libx264 --enable-shared
  libavutil      55. 78.100 / 55. 78.100
  libavcodec     57.107.100 / 57.107.100
  libavformat    57. 83.100 / 57. 83.100
  libavdevice    57. 10.100 / 57. 10.100
  libavfilter     6.107.100 /  6.107.100
  libavresample   3.  7.  0 /  3.  7.  0
  libswscale      4.  8.100 /  4.  8.100
  libswresample   2.  9.100 /  2.  9.100
  libpostproc    54.  7.100 / 54.  7.100
[f32le @ 0x7f6478000b80] Estimating duration from bitrate, this may be inaccurate
Input #0, f32le, from 'soundfile.bin':
  Duration: 00:00:04.00, bitrate: 1407 kb/s
    Stream #0:0: Audio: pcm_f32le, 44000 Hz, 1 channels, flt, 1408 kb/s
   3.90 M-A:  0.000 fd=   0 aq=    0KB vq=    0KB sq=    0B f=0/0
```

### Fretboard quizzer

Answer questions about the guitar fretboard.

Sample run:
```
FRETBOARD QUIZZER: What is the note on fret 11 of string 1 in standard tuning?
eb
FRETBOARD QUIZZER: Correct! Hit enter to continue or enter an arbitrary key to exit.

FRETBOARD QUIZZER: Where is the note G on string 1 in standard tuning?
3
FRETBOARD QUIZZER: Correct! Hit enter to continue or enter an arbitrary key to exit.

FRETBOARD QUIZZER: Where is the note Gs on string 1 in standard tuning?
3
FRETBOARD QUIZZER: Wrong. Hit enter to continue or enter an arbitrary key to exit.
e
```