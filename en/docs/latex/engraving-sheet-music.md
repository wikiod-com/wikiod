---
title: "Engraving Sheet Music"
slug: "engraving-sheet-music"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## LilyPond
The LilyPond notation engraver can be used with LaTeX via the `lilypond-book` command. First lets create a LaTeX document (with the file extension `.lytex`) to embed our music in:

    \documentclass[letterpaper,12pt]{article}
    
    \begin{document}
    
    \begin{center}
        {\fontsize{24pt}{24pt}\textbf{Twa Corbies}}\\
    \end{center}
    
    \begin{flushright}
        \textsc{Your Name}
    \end{flushright}
    
    % We don't need to require anything for this because lilypond-book will process it.
    \lilypondfile{TwaCorbies.ly}
    \end{document}

Then we create our LilyPond file (`.ly`), including the `lilypond-book-preamble.ly` file (which LilyPond will know how to find):

    \version "2.16.2"
    
    \include "lilypond-book-preamble.ly"
    
    voice = <<
        \relative c' {
            \tempo "con affetto"
            \clef bass
            \key e \minor
            \time 3/4
    
            a a b | c a a | g a2 |
            a4 a b | c2 ~ c8 a8 | a8 g a2 |
            \bar "|."
        }
        \addlyrics{
            As I was wal -- king all a -- lane
            I heard twa cor -- bies make a mane.
        }
    >>
    
    \score {
        <<
            \new Staff = "voice" {
                \voice
            }
        >>
        \layout { }
        \midi {
            \context {
                \Score
                tempoWholesPerMinute = #(ly:make-moment 90 4)
            }
        }
    }

to build, we then run the `lilypond-book` command:

    lilypond-book --include=mymusicsourcedirectory/ --pdf TwaCorbies.lytex

which will output a PDF containing your LilyPond engraved music:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/fvN9H.png

