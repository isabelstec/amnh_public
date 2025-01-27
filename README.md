put both files in the same directory as the config file

ghc multiphyg.hs
./multiphyg configFile dataPath stub distribution possiblePhygFile

ex: ./multiphyg IE-lang-Neyman.config ./IE-fastc/fasta stub exponential indfiles.pg

phyg file not needed, if not provided it'll make one with just the modelComplexity, read lines, build(distance, rdwag) and report
