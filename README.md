
there are un-released versions of attoparsec and aeson with better
error reporting:

git clone https://github.com/zerobuzz/attoparsec --branch better-errors+1
git clone https://github.com/maximkulkin/aeson --branch json-path-error-reporting

i'm not sure about the aeson errors having any effect when parsing
yaml files, but who knows?

if you want to force these two packages to be included, modify the
resp. version constraints in garnett.cabal.
