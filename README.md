# microformats2-parser [![Hackage](https://img.shields.io/hackage/v/microformats2-parser.svg?style=flat)](https://hackage.haskell.org/package/microformats2-parser) [![ISC License](https://img.shields.io/badge/license-ISC-red.svg?style=flat)](https://tldrlegal.com/license/-isc-license)

[Microformats 2] parser for Haskell!

Originally created for [sweetroll] :-)

The types are located in a separate package called [microformats2-types].

[Microformats 2]: http://microformats.org/wiki/microformats2
[sweetroll]: https://github.com/myfreeweb/sweetroll
[microformats2-types]: https://github.com/myfreeweb/microformats2-types

## Development

Use [stack] to build.  
Use ghci to run tests quickly with `:test` (see the `.ghci` file).

```bash
$ stack build

$ stack test && rm tests.tix

$ stack ghci --ghc-options="-fno-hpc"
```

[stack]: https://github.com/commercialhaskell/stack

## License

Copyright 2015 Greg V <greg@unrelenting.technology>  
Available under the ISC license, see the `COPYING` file
