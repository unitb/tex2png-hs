# tex2png-hs

[![Build Status](https://travis-ci.org/literate-unitb/tex2png-hs.svg?branch=master)](https://travis-ci.org/literate-unitb/tex2png-hs)

`tex2png-hs` is a Haskell port of Xyne's `tex2png` tool. It is a
wrapper around `latex` and `dvipng` and provides several options for
modifying its behaviour, such as cropping the whitespace around the
content, specifying the DPI, or inputting a full document.

To see all options, run `tex2png-hs --help` if you've already ran
`stack install`, or run `stack exec -- tex2png-hs --help` if you're
just experimenting.
