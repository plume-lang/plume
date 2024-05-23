# Plume releases changelog

All notable changes from Plume releases will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.7.2](https://github.com/plume-lang/plume/compare/0.7.1...0.7.2) - 2024-05-23

### Fixed

- fix(tc): removed unwanted print from typechecker [`#fix(tc): removed unwanted print from typechecker`](https://github.com/plume-lang/plume/issues/fix(tc): removed unwanted print from typechecker)
- fix(tc): fixed super-interface removal in typechecker [`#fix(tc): fixed super-interface removal in typechecker`](https://github.com/plume-lang/plume/issues/fix(tc): fixed super-interface removal in typechecker)
- fix(llir): fixed choice between store local and store global [`#fix(llir): fixed choice between store local and store global`](https://github.com/plume-lang/plume/issues/fix(llir): fixed choice between store local and store global)
- fix(compiler): fixed ADT-nested functions in switch removal [`#fix(compiler): fixed ADT-nested functions in switch removal`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed ADT-nested functions in switch removal)
- fix(cc): rewrote entirely closure conversion [`#fix(cc): rewrote entirely closure conversion`](https://github.com/plume-lang/plume/issues/fix(cc): rewrote entirely closure conversion)
- fix(tc): fixed superclass discharging [`#fix(tc): fixed superclass discharging`](https://github.com/plume-lang/plume/issues/fix(tc): fixed superclass discharging)
- fix(std): fixed standard library according to new typechecker errors [`#fix(std): fixed standard library according to new typechecker errors`](https://github.com/plume-lang/plume/issues/fix(std): fixed standard library according to new typechecker errors)
- fix(compiler): fixed closure conversion dictionary generation order [`#fix(compiler): fixed closure conversion dictionary generation order`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed closure conversion dictionary generation order)
- fix(tc): fixed drunkely error in extension and declaration typechecking [`#fix(tc): fixed drunkely error in extension and declaration typechecking`](https://github.com/plume-lang/plume/issues/fix(tc): fixed drunkely error in extension and declaration typechecking)
- fix(std): fixed standard with orphan extensions [`#fix(std): fixed standard with orphan extensions`](https://github.com/plume-lang/plume/issues/fix(std): fixed standard with orphan extensions)
- fix(tc): fixed extension typechecking by providing better error handling [`#fix(tc): fixed extension typechecking by providing better error handling`](https://github.com/plume-lang/plume/issues/fix(tc): fixed extension typechecking by providing better error handling)
- fix(parser): fixed orphan extension names [`#fix(parser): fixed orphan extension names`](https://github.com/plume-lang/plume/issues/fix(parser): fixed orphan extension names)
- fix(std): fixed standard library with new errors [`#fix(std): fixed standard library with new errors`](https://github.com/plume-lang/plume/issues/fix(std): fixed standard library with new errors)
- fix(main): removed temporarily memory module import [`#fix(main): removed temporarily memory module import`](https://github.com/plume-lang/plume/issues/fix(main): removed temporarily memory module import)
- fix(language): removed memory manager due to critical issue [`#fix(language): removed memory manager due to critical issue`](https://github.com/plume-lang/plume/issues/fix(language): removed memory manager due to critical issue)
- fix(compiler): fixed jumps, locals variables and return compilation [`#fix(compiler): fixed jumps, locals variables and return compilation`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed jumps, locals variables and return compilation)
- fix(tc): added default position for errors [`#fix(tc): added default position for errors`](https://github.com/plume-lang/plume/issues/fix(tc): added default position for errors)
- fix(tc): fixed local declaration typechecking with qualifiers [`#fix(tc): fixed local declaration typechecking with qualifiers`](https://github.com/plume-lang/plume/issues/fix(tc): fixed local declaration typechecking with qualifiers)
- fix(parser): optimized literal with string interpolation parsing [`#fix(parser): optimized literal with string interpolation parsing`](https://github.com/plume-lang/plume/issues/fix(parser): optimized literal with string interpolation parsing)

### Commits

- **Breaking change:** feat(language): added WIP memory management reference counting algorithm [`392691f`](https://github.com/plume-lang/plume/commit/392691f415323cf30b01a5e06ae367925afc5666)
- **Breaking change:** feat(compiler): removed dictionary resolving step [`59d3b33`](https://github.com/plume-lang/plume/commit/59d3b332fdbacc50c8d923d2c6bbd7417e0548bc)
- **Breaking change:** feat(std): added some new standard functions [`86bbac4`](https://github.com/plume-lang/plume/commit/86bbac4a8ab1bcfb8b596afcacb3f99224523030)
- **Breaking change:** feat(syntax): added useless block removal step [`399f4bb`](https://github.com/plume-lang/plume/commit/399f4bbf71106d652fbbc904fd277d717032f468)
- **Breaking change:** feat(lang): added support for orphan extensions [`3d65ab1`](https://github.com/plume-lang/plume/commit/3d65ab1a866c5773c4e877dd65beadc857f4d815)
- **Breaking change:** feat(std): added complex number datatype [`138d9e5`](https://github.com/plume-lang/plume/commit/138d9e59c9b4b3b3496ec4940076584fc7fdaf79)
- **Breaking change:** feat(std): added monadic interface [`ec0e8da`](https://github.com/plume-lang/plume/commit/ec0e8dae2f62ac43ebe9eb4f7a7cac39ced8f7f5)
- **Breaking change:** feat(tc): added new error messages for extensions [`26e33c8`](https://github.com/plume-lang/plume/commit/26e33c8b7190e9a34514e430273644676bde02e5)
- **Breaking change:** feat(std): added GC functions [`97a4647`](https://github.com/plume-lang/plume/commit/97a46478330609ef227104ea3afcecc8824b9571)
- **Breaking change:** feat(std): added product interface to fix orphan extension issue [`752d8ea`](https://github.com/plume-lang/plume/commit/752d8ea3c1822039574420407424c2a5d60e5139)
- **Breaking change:** feat(tc): added error handling for already defined function in instance [`660c6a8`](https://github.com/plume-lang/plume/commit/660c6a8debb899b6b062ba151d93294fc90f5892)
- **Breaking change:** feat(language): added support for multiple interface inheritance [`2e89641`](https://github.com/plume-lang/plume/commit/2e89641e2a4085f9653c09afa9799ef5ff60df6f)
- **Breaking change:** feat(type): added special compilation case for ADTs [`612bb3e`](https://github.com/plume-lang/plume/commit/612bb3e6b78d03cb16abaff1c33ee4d2f5d61fe7)
- **Breaking change:** feat(std): added list equality [`f3bd57c`](https://github.com/plume-lang/plume/commit/f3bd57c68ea59fac72abf5e8f0342e2a8744e3bd)
- **Breaking change:** feat(main): removed dictionary solving step from main entry [`d3d2542`](https://github.com/plume-lang/plume/commit/d3d2542088cbf68fe2b1f4881f3d08f70db432ed)
- **Breaking change:** feat(main): added dictionary resolving step in main app [`4300229`](https://github.com/plume-lang/plume/commit/4300229cdf082d0f7aad9bb2af2de2a4e8bd725a)
- **Breaking change:** feat(ffi): added more precise char_to_string error message [`cd7b5b2`](https://github.com/plume-lang/plume/commit/cd7b5b221c347a0fcad874c919dd38006253692b)
- **Breaking change:** feat(main): added block removal step [`4400c02`](https://github.com/plume-lang/plume/commit/4400c02158a43406a82d8747b7f6e39612139918)
- **Breaking change:** feat(main): added memory management step [`dff2ce5`](https://github.com/plume-lang/plume/commit/dff2ce5d2d3359cdf21fbeaa4af008635773c7b8)

## [0.7.1](https://github.com/plume-lang/plume/compare/0.7...0.7.1) - 2024-05-18

### Fixed

- fix(tc): fixed return check for functions without specified return-type [`#fix(tc): fixed return check for functions without specified return-type`](https://github.com/plume-lang/plume/issues/fix(tc): fixed return check for functions without specified return-type)
- fix(std): added missing returns [`#fix(std): added missing returns`](https://github.com/plume-lang/plume/issues/fix(std): added missing returns)
- fix(tc): fixed critical issue with super-interfaces order [`#fix(tc): fixed critical issue with super-interfaces order`](https://github.com/plume-lang/plume/issues/fix(tc): fixed critical issue with super-interfaces order)

### Commits

- **Breaking change:** feat(tc): added no-return security [`8e4cbe3`](https://github.com/plume-lang/plume/commit/8e4cbe37132ad5477b8785f3cac55b0ad84298b1)
- **Breaking change:** feat(syntax): added type aliases to Plume [`e6fd510`](https://github.com/plume-lang/plume/commit/e6fd510ba7573d931e7e9cd52c2f269cc28dd7e2)
- refactor(compiler): improved compiler errors in assembler [`737bf27`](https://github.com/plume-lang/plume/commit/737bf2729c8eb049e0d93b0f337caa4cfab2e782)

## [0.7](https://github.com/plume-lang/plume/compare/0.6.6...0.7) - 2024-05-17

### Fixed

- fix(assembler): fixed condition jump optimization [`#fix(assembler): fixed condition jump optimization`](https://github.com/plume-lang/plume/issues/fix(assembler): fixed condition jump optimization)
- fix(compiler): fixed library compilation to bytecode [`#fix(compiler): fixed library compilation to bytecode`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed library compilation to bytecode)
- fix(parser): fixed operator functions being not parsed [`#fix(parser): fixed operator functions being not parsed`](https://github.com/plume-lang/plume/issues/fix(parser): fixed operator functions being not parsed)
- fix(compiler): fixed LLIR compilation [`#fix(compiler): fixed LLIR compilation`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed LLIR compilation)
- fix(tc): fixed type-extension generics for declarations [`#fix(tc): fixed type-extension generics for declarations`](https://github.com/plume-lang/plume/issues/fix(tc): fixed type-extension generics for declarations)
- fix(tc): removed Instance type [`#fix(tc): removed Instance type`](https://github.com/plume-lang/plume/issues/fix(tc): removed Instance type)

### Commits

- **Breaking change:** feat(compiler): added Low Level IR compilation step [`682ed38`](https://github.com/plume-lang/plume/commit/682ed382909687accfc50949eb2c7552a2712d14)
- **Breaking change:** feat(tc): added new generic and type extension representation [`132ac36`](https://github.com/plume-lang/plume/commit/132ac3605ee6dbcc45aad7c81819a459ec4bcef1)
- **Breaking change:** feat(std): rewritten standard library with new extension system [`ef74346`](https://github.com/plume-lang/plume/commit/ef743465346ebfbe3966dc3aafe59ea9d5d7556b)
- **Breaking change:** feat(tc): re-created extension typechecking [`0ddb967`](https://github.com/plume-lang/plume/commit/0ddb9676e00a49dd841446eaebb1c3e4cdc2f945)
- **Breaking change:** feat(tc): updated all checker modules with new type-class API [`f86d20a`](https://github.com/plume-lang/plume/commit/f86d20ab376acc0adb97695e05fe7df8a15fc9ea)
- **Breaking change:** feat(typeclass): added some type-class related resolution functions [`8289dd4`](https://github.com/plume-lang/plume/commit/8289dd44b33d447dbbe81d557696ad5a2f186c97)
- **Breaking change:** feat(compiler): added final bytecode assembler [`7070ec2`](https://github.com/plume-lang/plume/commit/7070ec20eedbeb1852100a29e786a5fa0addc7b4)
- **Breaking change:** feat: updated constraints module with new type-class API [`7b48b58`](https://github.com/plume-lang/plume/commit/7b48b58bd32ef5e1f7bdf9238d2b4e286f31cadb)
- **Breaking change:** feat(compiler): added bytecode serialization step [`a057e70`](https://github.com/plume-lang/plume/commit/a057e702261c4c16001eeaf9f2f4c777fd8fc96b)
- **Breaking change:** feat(syntax): added new interface keyword [`1d178c1`](https://github.com/plume-lang/plume/commit/1d178c1a1e511db1bba8f83b8ff8ffb03a3d53b4)
- **Breaking change:** feat(compiler): added label transformation pass [`d98c5ea`](https://github.com/plume-lang/plume/commit/d98c5eab2e2033b9769184cb9d7e8f3383de3632)
- **Breaking change:** feat(tc): added interface type-checking [`fd22a38`](https://github.com/plume-lang/plume/commit/fd22a38d54b0498105dcf5d6d4da8b16290534d7)
- **Breaking change:** feat(lib): added string color library [`e9592b5`](https://github.com/plume-lang/plume/commit/e9592b5581de9691b7442c98935b7cacede319e8)
- **Breaking change:** feat(tlir): added some instance related expressions [`38a0831`](https://github.com/plume-lang/plume/commit/38a0831c302cec5ebfdc88215beaa58b6504c575)
- **Breaking change:** feat(parser): added basic string interpolation parsing [`af501dc`](https://github.com/plume-lang/plume/commit/af501dc845ecca769f1780227bd9460d9e6999e1)
- **Breaking change:** feat: updated main entry app [`c844626`](https://github.com/plume-lang/plume/commit/c8446262db38df99edd0937fa7b39140588e28af)
- **Breaking change:** feat(std): added standard int_to_str function [`86a2095`](https://github.com/plume-lang/plume/commit/86a209550c3328cc7c14077d2af09c49c90013b1)
- **Breaking change:** feat(compiler): added instance pre-compilation in type erasure step [`dc4b6cb`](https://github.com/plume-lang/plume/commit/dc4b6cb8fedf104047638523fabc10e861eab10a)
- **Breaking change:** feat(compiler): added default unit returning in functions assembler [`a6e45b8`](https://github.com/plume-lang/plume/commit/a6e45b8631915bb2bfe30cebf72de3a8a54c13f5)
- **Breaking change:** feat(std): added to_str extension for string type [`b163582`](https://github.com/plume-lang/plume/commit/b163582b767e0dfbeaa43a804f62803518d759f3)
- **Breaking change:** feat(lib): added withRef IO function [`9c57bcf`](https://github.com/plume-lang/plume/commit/9c57bcf1e0bc0fc969a2289359fc7f37bfde89c6)
- **Breaking change:** feat(main): re-added standard library compilation to toolchain [`9d90132`](https://github.com/plume-lang/plume/commit/9d901326acca9fc701e4adb93bb688330d13b737)

## [0.6.6](https://github.com/plume-lang/plume/compare/0.6.5...0.6.6) - 2024-05-04

### Fixed

- fix(error): fixed error handling to stdout instead of stderr [`#fix(error): fixed error handling to stdout instead of stderr`](https://github.com/plume-lang/plume/issues/fix(error): fixed error handling to stdout instead of stderr)

### Commits

- docs(example): added basic error code [`efad62c`](https://github.com/plume-lang/plume/commit/efad62c3b05b1c7a49e08e1365d81fc8bab86c86)

## [0.6.5](https://github.com/plume-lang/plume/compare/0.6.4...0.6.5) - 2024-04-30

### Fixed

- fix(xmake): removed multiple rules in standard build [`#fix(xmake): removed multiple rules in standard build`](https://github.com/plume-lang/plume/issues/fix(xmake): removed multiple rules in standard build)

### Commits

- **Breaking change:** feat(vm): introduced local negative index lookup resolution [`cd11ac3`](https://github.com/plume-lang/plume/commit/cd11ac3957af082668e4c0a5055d2a94a6c59bf6)
- build(git): updated runtime submodule [`0fe0364`](https://github.com/plume-lang/plume/commit/0fe0364eadb632927f53e858c374877addcb9aaa)

## [0.6.4](https://github.com/plume-lang/plume/compare/0.6.3...0.6.4) - 2024-04-29

### Commits

- build(git): updated runtime submodule [`5fef5e6`](https://github.com/plume-lang/plume/commit/5fef5e63c6ee46b91489ee10c17b9f0c6d453e67)

## [0.6.3](https://github.com/plume-lang/plume/compare/0.6.2...0.6.3) - 2024-04-28

### Fixed

- fix(tc): fixed unification direction [`#fix(tc): fixed unification direction`](https://github.com/plume-lang/plume/issues/fix(tc): fixed unification direction)
- fix(encoding): fixed encoding with conditional macro resolution [`#fix(encoding): fixed encoding with conditional macro resolution`](https://github.com/plume-lang/plume/issues/fix(encoding): fixed encoding with conditional macro resolution)

## [0.6.2](https://github.com/plume-lang/plume/compare/0.6.1...0.6.2) - 2024-04-28

### Fixed

- fix(encoding): fixed Windows bad encoding issue [`#fix(encoding): fixed Windows bad encoding issue`](https://github.com/plume-lang/plume/issues/fix(encoding): fixed Windows bad encoding issue)

## [0.6.1](https://github.com/plume-lang/plume/compare/0.6...0.6.1) - 2024-04-27

### Fixed

- fix(ci): fixed github ci overwriting cabal file [`#fix(ci): fixed github ci overwriting cabal file`](https://github.com/plume-lang/plume/issues/fix(ci): fixed github ci overwriting cabal file)

### Commits

- **Breaking change:** feat(tc/wip): working on practical redundancy checking [`7cdaa31`](https://github.com/plume-lang/plume/commit/7cdaa31b3475ac48cece3d56c4b810306e2089f5)
- build(ci): added cabal configure fix [`484d0b4`](https://github.com/plume-lang/plume/commit/484d0b472b1bd9a7b23094810003ffd5a01e0ea7)

## [0.6](https://github.com/plume-lang/plume/compare/0.5.2...0.6) - 2024-04-27

### Fixed

- fix(std): removed closure types from ffi [`#fix(std): removed closure types from ffi`](https://github.com/plume-lang/plume/issues/fix(std): removed closure types from ffi)
- fix(std): fixed redudance issue with some list operations [`#fix(std): fixed redudance issue with some list operations`](https://github.com/plume-lang/plume/issues/fix(std): fixed redudance issue with some list operations)

### Commits

- **Breaking change:** feat(tc): added exhaustiveness and redundance checks [`14c3e96`](https://github.com/plume-lang/plume/commit/14c3e9610b60436d9d39669b288abf42630ce262)
- **Breaking change:** feat(error): improved error messages in Plume [`cacd65b`](https://github.com/plume-lang/plume/commit/cacd65bae73b94cb26f71ed696292e8d280fa69d)
- **Breaking change:** feat(plume): added variable extension member support [`58c27ef`](https://github.com/plume-lang/plume/commit/58c27ef13886c2cd53992deb118e21715d6bf76f)
- **Breaking change:** feat(error): added better error handling utility functions [`f5769c5`](https://github.com/plume-lang/plume/commit/f5769c584bd9d39e6b7aaf1224a687782887a6c5)
- **Breaking change:** feat(tc): added better return errors [`e4a9c3f`](https://github.com/plume-lang/plume/commit/e4a9c3fe01cc527af333e88717c90904733e8c6f)
- **Breaking change:** feat(error): added better parser error backtracking [`b8d60a1`](https://github.com/plume-lang/plume/commit/b8d60a1049c896c122ec20f980880b1846692a11)
- **Breaking change:** feat(pretty): added warning thrower function [`633b284`](https://github.com/plume-lang/plume/commit/633b28422c8d9d14a3c214c21c587b3cde778ffe)

## [0.5.2](https://github.com/plume-lang/plume/compare/0.5.1...0.5.2) - 2024-04-25

### Fixed

- fix(ci): removed CLang installation on macOS [`#fix(ci): removed CLang installation on macOS`](https://github.com/plume-lang/plume/issues/fix(ci): removed CLang installation on macOS)

## [0.5.1](https://github.com/plume-lang/plume/compare/0.5...0.5.1) - 2024-04-25

### Fixed

- fix(xmake): added system=false flag for libcurl package [`#fix(xmake): added system=false flag for libcurl package`](https://github.com/plume-lang/plume/issues/fix(xmake): added system=false flag for libcurl package)
- fix(compiler): fixed temporaly dead code elmimination critical issue [`#fix(compiler): fixed temporaly dead code elmimination critical issue`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed temporaly dead code elmimination critical issue)

### Commits

- build(ci): added CLang installation in build action [`b7ab524`](https://github.com/plume-lang/plume/commit/b7ab524613eb3df26e31e1dac3749dbb5b2c05c5)
- build(ci): fixed CLang action on x64 macOS [`85f3fe2`](https://github.com/plume-lang/plume/commit/85f3fe2925886981428f5c553551245063ac9e1a)
- build(git): updated runtime submodule [`4d717d6`](https://github.com/plume-lang/plume/commit/4d717d639fb064957fe35134199d86483d47c82b)

## [0.5](https://github.com/plume-lang/plume/compare/0.4.2...0.5) - 2024-04-25

### Fixed

- fix(script): fixed root build style in python script [`#fix(script): fixed root build style in python script`](https://github.com/plume-lang/plume/issues/fix(script): fixed root build style in python script)
- fix(std): added missing type cases in print_helper [`#fix(std): added missing type cases in print_helper`](https://github.com/plume-lang/plume/issues/fix(std): added missing type cases in print_helper)
- fix(bench): fixed benchmark runner script compatibility [`#fix(bench): fixed benchmark runner script compatibility`](https://github.com/plume-lang/plume/issues/fix(bench): fixed benchmark runner script compatibility)

### Commits

- **Breaking change:** feat(perf): rewritten typechecker using HM algorithm J [`254d214`](https://github.com/plume-lang/plume/commit/254d21403e2f2e26afa9f4f1313c3ce58b8dc52c)
- **Breaking change:** feat(std): updated standard library with new runtime API [`ce1c626`](https://github.com/plume-lang/plume/commit/ce1c626624088bac98562947202786ce082c7d16)
- **Breaking change:** feat(perf): added arithmetic function inlining [`8f8099e`](https://github.com/plume-lang/plume/commit/8f8099e23de1b4094a11f17f45e9ba594cc61a03)
- **Breaking change:** feat(compiler): adapted native compilation to new syntax API [`6c83a20`](https://github.com/plume-lang/plume/commit/6c83a20ef1a3007cb2eabfbbb23612aa2deae6ea)
- **Breaking change:** feat(compiler): updated type erasure with new typechecker API [`f5e2e08`](https://github.com/plume-lang/plume/commit/f5e2e08cdbca76ad4ea61a2332038c63d54c5fc5)
- **Breaking change:** feat(translation): removed absolute path resolution for natives [`3fea52e`](https://github.com/plume-lang/plume/commit/3fea52ea6c1efb259aff47850ce7de38a5e0eca1)
- **Breaking change:** feat(common): added generic name getter function [`c263bc3`](https://github.com/plume-lang/plume/commit/c263bc348b914424dd893f7e2afce21550f8fa37)
- **Breaking change:** feat(std): updated library extension [`ccdff87`](https://github.com/plume-lang/plume/commit/ccdff875a7dc7dbfaf69d5c9188217390eb045c1)

## [0.4.2](https://github.com/plume-lang/plume/compare/0.4.1...0.4.2) - 2024-04-16

### Fixed

- fix(script): fixed project builder python script [`#fix(script): fixed project builder python script`](https://github.com/plume-lang/plume/issues/fix(script): fixed project builder python script)
- fix(script): improved xmake build [`#fix(script): improved xmake build`](https://github.com/plume-lang/plume/issues/fix(script): improved xmake build)
- fix(translation): fixed bad-path local passing [`#fix(translation): fixed bad-path local passing`](https://github.com/plume-lang/plume/issues/fix(translation): fixed bad-path local passing)

### Commits

- build(script): added xmake fetching command [`7b2e499`](https://github.com/plume-lang/plume/commit/7b2e499af1d61c3df5768ce43e18c17015434b37)
- build(ci): added dependency installation [`9477cdf`](https://github.com/plume-lang/plume/commit/9477cdfe22e5ac90336641624954b540302e5aaa)

## [0.4.1](https://github.com/plume-lang/plume/compare/0.4...0.4.1) - 2024-04-16

### Fixed

- fix(script): fixed xmake build in python build script [`#fix(script): fixed xmake build in python build script`](https://github.com/plume-lang/plume/issues/fix(script): fixed xmake build in python build script)

## [0.4](https://github.com/plume-lang/plume/compare/0.3...0.4) - 2024-04-16

### Fixed

- fix(parser): fixed char literal parsing [`#fix(parser): fixed char literal parsing`](https://github.com/plume-lang/plume/issues/fix(parser): fixed char literal parsing)
- fix(std): fixed string concatenation [`#fix(std): fixed string concatenation`](https://github.com/plume-lang/plume/issues/fix(std): fixed string concatenation)
- fix(parser): fixed space consumer on toplevels [`#fix(parser): fixed space consumer on toplevels`](https://github.com/plume-lang/plume/issues/fix(parser): fixed space consumer on toplevels)
- fix(compiler): fixed return insertion [`#fix(compiler): fixed return insertion`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed return insertion)
- fix(tc): fixed return type insertion in extension member [`#fix(tc): fixed return type insertion in extension member`](https://github.com/plume-lang/plume/issues/fix(tc): fixed return type insertion in extension member)
- fix(tc): fixed return typechecking in blocks [`#fix(tc): fixed return typechecking in blocks`](https://github.com/plume-lang/plume/issues/fix(tc): fixed return typechecking in blocks)
- fix(parser): fixed char literal parsing [`#fix(parser): fixed char literal parsing`](https://github.com/plume-lang/plume/issues/fix(parser): fixed char literal parsing)
- fix(std): removed use of string interpolation temporarily [`#fix(std): removed use of string interpolation temporarily`](https://github.com/plume-lang/plume/issues/fix(std): removed use of string interpolation temporarily)
- fix(perf): inlined some substitution related functions [`#fix(perf): inlined some substitution related functions`](https://github.com/plume-lang/plume/issues/fix(perf): inlined some substitution related functions)
- fix(perf): removed use of lists and most useless try combinators [`#fix(perf): removed use of lists and most useless try combinators`](https://github.com/plume-lang/plume/issues/fix(perf): removed use of lists and most useless try combinators)
- fix(monad): removed reader stack monad from parser [`#fix(monad): removed reader stack monad from parser`](https://github.com/plume-lang/plume/issues/fix(monad): removed reader stack monad from parser)
- fix(markdown): fixed contributing table of contents [`#fix(markdown): fixed contributing table of contents`](https://github.com/plume-lang/plume/issues/fix(markdown): fixed contributing table of contents)
- fix(parser): added generics parsing for functions [`#fix(parser): added generics parsing for functions`](https://github.com/plume-lang/plume/issues/fix(parser): added generics parsing for functions)
- fix(tc): fixed function closure application in typechecker [`#fix(tc): fixed function closure application in typechecker`](https://github.com/plume-lang/plume/issues/fix(tc): fixed function closure application in typechecker)
- fix(std): fixed unhandled case in IO module [`#fix(std): fixed unhandled case in IO module`](https://github.com/plume-lang/plume/issues/fix(std): fixed unhandled case in IO module)
- fix(example): fixed example syntax [`#fix(example): fixed example syntax`](https://github.com/plume-lang/plume/issues/fix(example): fixed example syntax)
- fix(std): updated all STD with new syntax [`#fix(std): updated all STD with new syntax`](https://github.com/plume-lang/plume/issues/fix(std): updated all STD with new syntax)
- fix(monad): fixed monad parser initial position [`#fix(monad): fixed monad parser initial position`](https://github.com/plume-lang/plume/issues/fix(monad): fixed monad parser initial position)
- fix(desugaring): fixed duplicated switch variable [`#fix(desugaring): fixed duplicated switch variable`](https://github.com/plume-lang/plume/issues/fix(desugaring): fixed duplicated switch variable)
- fix(compiler): fixed pattern matching AND issue [`#fix(compiler): fixed pattern matching AND issue`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed pattern matching AND issue)
- fix(desugar): fixed variable conflict with pattern [`#fix(desugar): fixed variable conflict with pattern`](https://github.com/plume-lang/plume/issues/fix(desugar): fixed variable conflict with pattern)
- fix(desugaring): fixed switch condition desugaring [`#fix(desugaring): fixed switch condition desugaring`](https://github.com/plume-lang/plume/issues/fix(desugaring): fixed switch condition desugaring)
- fix(compiler): improved return insertion in single statement blocks [`#fix(compiler): improved return insertion in single statement blocks`](https://github.com/plume-lang/plume/issues/fix(compiler): improved return insertion in single statement blocks)
- fix(tc): fixed extension calls by creating constraints [`#fix(tc): fixed extension calls by creating constraints`](https://github.com/plume-lang/plume/issues/fix(tc): fixed extension calls by creating constraints)
- fix(parser): fixed tuple parsing conflict [`#fix(parser): fixed tuple parsing conflict`](https://github.com/plume-lang/plume/issues/fix(parser): fixed tuple parsing conflict)
- fix(logging): removed useless print [`#fix(logging): removed useless print`](https://github.com/plume-lang/plume/issues/fix(logging): removed useless print)
- fix(tc): fixed constraint solving on extensions with same parent generics [`#fix(tc): fixed constraint solving on extensions with same parent generics`](https://github.com/plume-lang/plume/issues/fix(tc): fixed constraint solving on extensions with same parent generics)
- fix(tc): fixed critical issue with local constraint solving [`#fix(tc): fixed critical issue with local constraint solving`](https://github.com/plume-lang/plume/issues/fix(tc): fixed critical issue with local constraint solving)
- fix(typo): fixed some big typo issues in readme [`#fix(typo): fixed some big typo issues in readme`](https://github.com/plume-lang/plume/issues/fix(typo): fixed some big typo issues in readme)

### Commits

- **Breaking change:** feat(parser): added and updated some parser modules [`60dfe61`](https://github.com/plume-lang/plume/commit/60dfe614a0c045b71ba8f9a66bb78b5b590d70dc)
- **Breaking change:** feat(parser): added new Plume syntax parser [`2f672f6`](https://github.com/plume-lang/plume/commit/2f672f6c54297fc75916e1f1c11e7d93e014525f)
- **Breaking change:** feat(std): added many functions to standard datatypes [`8cfa892`](https://github.com/plume-lang/plume/commit/8cfa8928931f99476fc53a7d26732610d5a1b227)
- **Breaking change:** feat(compiler): implemented mutability in compiler [`a7d6ce4`](https://github.com/plume-lang/plume/commit/a7d6ce467ec2c0a852e3824f99e20f495f1a7a19)
- **Breaking change:** feat(compiler): added closure mutability support [`ec4558e`](https://github.com/plume-lang/plume/commit/ec4558e0f2a56dc1b5bf2d26a3ec955605f8ef59)
- **Breaking change:** feat(translation): using SortedList instead of Haskell lists [`a883c13`](https://github.com/plume-lang/plume/commit/a883c13dad4d490e1e3b17a960a755ae950d0165)
- **Breaking change:** feat(parser): added mutable declarations [`bd825a9`](https://github.com/plume-lang/plume/commit/bd825a96f61af1def2c3958d9712a7d22660d04c)
- **Breaking change:** feat(tc): implemented mutability checking [`54b41a1`](https://github.com/plume-lang/plume/commit/54b41a1540dfb16b35321c81cc2a15ba40009b2d)
- **Breaking change:** feat(std): added some mutability examples [`950000d`](https://github.com/plume-lang/plume/commit/950000dfdde81fcfe65175e723d2b6088542cb69)
- **Breaking change:** feat(std): replaced list slice with ffi call [`ace208c`](https://github.com/plume-lang/plume/commit/ace208ccf1c23eeeb9f8b0c4836f3223748d034e)
- **Breaking change:** feat(std/io): organized standard IO library [`c62304a`](https://github.com/plume-lang/plume/commit/c62304a76d3e4ef2ac565a060c44937b6605548c)
- **Breaking change:** feat(parser): added mutability field access shortcut [`eccad0d`](https://github.com/plume-lang/plume/commit/eccad0d02e11d107e3573ce995f539ee0141e6e6)
- **Breaking change:** feat(std): added fetch function [`7993693`](https://github.com/plume-lang/plume/commit/79936935222d6a6975054db5d8cdb2e8a05756cf)
- **Breaking change:** feat(plume): implemented unmut operation checking and compilation [`98b910a`](https://github.com/plume-lang/plume/commit/98b910afddd148a0c7c44872970a6250ee5d0a1b)
- **Breaking change:** feat(std): added to_string mutable case [`587b837`](https://github.com/plume-lang/plume/commit/587b837023005856c8b64e4b77b47807b0c2d4d1)
- **Breaking change:** feat(ffi): added make_err and make_ok functions [`829da99`](https://github.com/plume-lang/plume/commit/829da995b9e7e09e65205e5746e95287bd14734c)
- **Breaking change:** feat(tc): added datatype information ref [`71becce`](https://github.com/plume-lang/plume/commit/71becceee19ddbc291c65ef1bcd872cad0def103)
- **Breaking change:** feat(ffi): added make_unit function [`76c0989`](https://github.com/plume-lang/plume/commit/76c098953cb775520d6456c5f55d7fc9ab25b68b)
- **Breaking change:** feat(std): added input function [`3ed0fff`](https://github.com/plume-lang/plume/commit/3ed0fffdeb7d1a27ff11e93b524169aad8a1ff70)
- **Breaking change:** feat(parser): added unmut operator [`22a831c`](https://github.com/plume-lang/plume/commit/22a831c0da656b2c634e5787e71cf67f9f2fca9a)
- **Breaking change:** feat(parser): added ability to parse indented lists [`96085f8`](https://github.com/plume-lang/plume/commit/96085f8cf6ff9777f24463eb94af6565fbbbdc63)
- **Breaking change:** feat(tc): added error on already defined variable [`21cdc1c`](https://github.com/plume-lang/plume/commit/21cdc1c8751bcd21f594e06ff4a012592b93b2f5)
- **Breaking change:** feat(monad): added parseTest function [`5a4ecc3`](https://github.com/plume-lang/plume/commit/5a4ecc318aecacfe75007323f14932f4a84d574c)
- **Breaking change:** feat(std): added not equal operator on int and char [`35304b6`](https://github.com/plume-lang/plume/commit/35304b6cd43103842711f28f7f5b5770e6175bd7)
- **Breaking change:** feat(tc): added return type backtrack to local [`69117ab`](https://github.com/plume-lang/plume/commit/69117abaa47697d5b1267dc83aee9f8cd7a9c752)

## [0.3](https://github.com/plume-lang/plume/compare/0.2...0.3) - 2024-04-06

### Fixed

- fix(std): fixed int slice [`#fix(std): fixed int slice`](https://github.com/plume-lang/plume/issues/fix(std): fixed int slice)
- fix(parser): fixed tuple and closure priority [`#fix(parser): fixed tuple and closure priority`](https://github.com/plume-lang/plume/issues/fix(parser): fixed tuple and closure priority)
- fix(std): fixed DLL compilation on windows [`#fix(std): fixed DLL compilation on windows`](https://github.com/plume-lang/plume/issues/fix(std): fixed DLL compilation on windows)
- fix(parser): fixed postfix slicing [`#fix(parser): fixed postfix slicing`](https://github.com/plume-lang/plume/issues/fix(parser): fixed postfix slicing)
- fix(std): fixed equality operator on char [`#fix(std): fixed equality operator on char`](https://github.com/plume-lang/plume/issues/fix(std): fixed equality operator on char)
- fix(compiler): fixed return insertion and dead code analysis [`#fix(compiler): fixed return insertion and dead code analysis`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed return insertion and dead code analysis)
- fix(tc): fixed closure return type checking [`#fix(tc): fixed closure return type checking`](https://github.com/plume-lang/plume/issues/fix(tc): fixed closure return type checking)
- fix(parser): fixed import parsing in import parser [`#fix(parser): fixed import parsing in import parser`](https://github.com/plume-lang/plume/issues/fix(parser): fixed import parsing in import parser)
- fix(plume): added native function duplicata check [`#fix(plume): added native function duplicata check`](https://github.com/plume-lang/plume/issues/fix(plume): added native function duplicata check)
- fix(tc): fixed issue with duplicated extensions [`#fix(tc): fixed issue with duplicated extensions`](https://github.com/plume-lang/plume/issues/fix(tc): fixed issue with duplicated extensions)

### Commits

- **Breaking change:** feat(std): added indexing methods on str and lists [`d449ce5`](https://github.com/plume-lang/plume/commit/d449ce5f0baba6ba54e7aabbab315e8b998c9d5c)
- **Breaking change:** feat(std): added slicing methods for str and list [`ece23e8`](https://github.com/plume-lang/plume/commit/ece23e88890dcfdee8dc396f1580d64dad665d9e)
- **Breaking change:** feat(syntax): added index slicing to plume [`e1cd8b4`](https://github.com/plume-lang/plume/commit/e1cd8b444eea328ae6e428db1dc5a8def24339d7)
- **Breaking change:** feat(parser): added indexing expression [`8df1839`](https://github.com/plume-lang/plume/commit/8df18397a01c85ddd281362dd68b6f41e1a79bc4)
- **Breaking change:** feat(std): added some char related methods [`4dd0f21`](https://github.com/plume-lang/plume/commit/4dd0f2140af7edcb986b5ad296af9c540784e6dc)
- **Breaking change:** feat(syntax): replaced colon with double dot for slicing [`2c22f85`](https://github.com/plume-lang/plume/commit/2c22f855bc967b1802da1434debdeb97ab8209ac)

## [0.2](https://github.com/plume-lang/plume/compare/0.1...0.2) - 2024-04-04

### Fixed

- fix(script): fixed another issue about file extension [`#fix(script): fixed another issue about file extension`](https://github.com/plume-lang/plume/issues/fix(script): fixed another issue about file extension)
- fix(script): fixed dumb build_project script error [`#fix(script): fixed dumb build_project script error`](https://github.com/plume-lang/plume/issues/fix(script): fixed dumb build_project script error)
- fix(cabal): fixed cabal package name causing action to fail [`#fix(cabal): fixed cabal package name causing action to fail`](https://github.com/plume-lang/plume/issues/fix(cabal): fixed cabal package name causing action to fail)
- fix(std): fixed println and execute_command signatures [`#fix(std): fixed println and execute_command signatures`](https://github.com/plume-lang/plume/issues/fix(std): fixed println and execute_command signatures)
- fix(compiler): fixed useless nil return statements [`#fix(compiler): fixed useless nil return statements`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed useless nil return statements)
- fix(cfg): fixed compilation issue with statements as expression [`#fix(cfg): fixed compilation issue with statements as expression`](https://github.com/plume-lang/plume/issues/fix(cfg): fixed compilation issue with statements as expression)
- fix(tc): added local constraint solving on extension methods [`#fix(tc): added local constraint solving on extension methods`](https://github.com/plume-lang/plume/issues/fix(tc): added local constraint solving on extension methods)
- fix(desugar): fixed return overuse in switch resolution [`#fix(desugar): fixed return overuse in switch resolution`](https://github.com/plume-lang/plume/issues/fix(desugar): fixed return overuse in switch resolution)

### Commits

- **Breaking change:** feat(parser): added native group definition support [`a1ed222`](https://github.com/plume-lang/plume/commit/a1ed2223e203bf6b12ef48788d10888f31ec8152)
- **Breaking change:** feat(parser): added indented dot chaining operator support [`af7f1fe`](https://github.com/plume-lang/plume/commit/af7f1fe87a4204d81af1c754ae5655b6e1d2979c)
- **Breaking change:** feat(std): removed dylib extension in standard library [`575c695`](https://github.com/plume-lang/plume/commit/575c6959a4632d1a95af84e9059712cf306fd6c5)
- **Breaking change:** feat(ffi): removed need of ffi file extension [`518306b`](https://github.com/plume-lang/plume/commit/518306b8c25a26f3ef304e698f89b380be459b69)

## 0.1 - 2024-04-02

### Fixed

- fix(ci): fixed permission on build action [`#fix(ci): fixed permission on build action`](https://github.com/plume-lang/plume/issues/fix(ci): fixed permission on build action)
- fix(ci): added missing release [`#fix(ci): added missing release`](https://github.com/plume-lang/plume/issues/fix(ci): added missing release)
- fix(ci): fixed missing comma in Compress-Archive [`#fix(ci): fixed missing comma in Compress-Archive`](https://github.com/plume-lang/plume/issues/fix(ci): fixed missing comma in Compress-Archive)
- fix(scripts): fixed f-string issue with pypy 3.10 [`#fix(scripts): fixed f-string issue with pypy 3.10`](https://github.com/plume-lang/plume/issues/fix(scripts): fixed f-string issue with pypy 3.10)
- fix(ffi): fixed portability for file stats functions [`#fix(ffi): fixed portability for file stats functions`](https://github.com/plume-lang/plume/issues/fix(ffi): fixed portability for file stats functions)
- fix(action): removed cc compiler test [`#fix(action): removed cc compiler test`](https://github.com/plume-lang/plume/issues/fix(action): removed cc compiler test)
- fix(compiler): fixed local reset on function definition [`#fix(compiler): fixed local reset on function definition`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed local reset on function definition)
- fix(tc): fixed extension solving [`#fix(tc): fixed extension solving`](https://github.com/plume-lang/plume/issues/fix(tc): fixed extension solving)
- fix(tc): fixed constraint solving on extensions [`#fix(tc): fixed constraint solving on extensions`](https://github.com/plume-lang/plume/issues/fix(tc): fixed constraint solving on extensions)
- fix(parser): now parsing correctly operators across modules [`#fix(parser): now parsing correctly operators across modules`](https://github.com/plume-lang/plume/issues/fix(parser): now parsing correctly operators across modules)
- fix(solver): introduced cyclic extension solving [`#fix(solver): introduced cyclic extension solving`](https://github.com/plume-lang/plume/issues/fix(solver): introduced cyclic extension solving)
- fix(extensions): fixed extension type erasure code ordering [`#fix(extensions): fixed extension type erasure code ordering`](https://github.com/plume-lang/plume/issues/fix(extensions): fixed extension type erasure code ordering)
- fix(assembler): fixed global variable storage in local scopes [`#fix(assembler): fixed global variable storage in local scopes`](https://github.com/plume-lang/plume/issues/fix(assembler): fixed global variable storage in local scopes)
- fix(compiler): fixed return-less ifs [`#fix(compiler): fixed return-less ifs`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed return-less ifs)
- fix(lexer): fixed non indented parser [`#fix(lexer): fixed non indented parser`](https://github.com/plume-lang/plume/issues/fix(lexer): fixed non indented parser)
- fix(tc): fixed plume typechecker return type [`#fix(tc): fixed plume typechecker return type`](https://github.com/plume-lang/plume/issues/fix(tc): fixed plume typechecker return type)
- fix(script): fixed python build and compile scripts [`#fix(script): fixed python build and compile scripts`](https://github.com/plume-lang/plume/issues/fix(script): fixed python build and compile scripts)
- fix(module): added standard module resolution [`#fix(module): added standard module resolution`](https://github.com/plume-lang/plume/issues/fix(module): added standard module resolution)
- fix(plume): removed some buggy features in extension type system [`#fix(plume): removed some buggy features in extension type system`](https://github.com/plume-lang/plume/issues/fix(plume): removed some buggy features in extension type system)
- fix(bytecode): removed useless instructions [`#fix(bytecode): removed useless instructions`](https://github.com/plume-lang/plume/issues/fix(bytecode): removed useless instructions)
- fix(compiler): fixed critical issue with functions [`#fix(compiler): fixed critical issue with functions`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed critical issue with functions)
- fix(compiler): started fixing extension compilation [`#fix(compiler): started fixing extension compilation`](https://github.com/plume-lang/plume/issues/fix(compiler): started fixing extension compilation)
- fix(runtime): removed old runtime code [`#fix(runtime): removed old runtime code`](https://github.com/plume-lang/plume/issues/fix(runtime): removed old runtime code)
- fix(compiler): fixed pattern match removal [`#fix(compiler): fixed pattern match removal`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed pattern match removal)
- fix(tc): removed useless print call [`#fix(tc): removed useless print call`](https://github.com/plume-lang/plume/issues/fix(tc): removed useless print call)
- fix(compiler): big fix of some compiler issues [`#fix(compiler): big fix of some compiler issues`](https://github.com/plume-lang/plume/issues/fix(compiler): big fix of some compiler issues)
- fix(tc): fixed typechecker issue with blocks [`#fix(tc): fixed typechecker issue with blocks`](https://github.com/plume-lang/plume/issues/fix(tc): fixed typechecker issue with blocks)
- fix(compiler): fixed some of backend passes [`#fix(compiler): fixed some of backend passes`](https://github.com/plume-lang/plume/issues/fix(compiler): fixed some of backend passes)
- fix(translation): added missing List case in translation [`#fix(translation): added missing List case in translation`](https://github.com/plume-lang/plume/issues/fix(translation): added missing List case in translation)
- fix(tc): fixed generic remapping on declarations [`#fix(tc): fixed generic remapping on declarations`](https://github.com/plume-lang/plume/issues/fix(tc): fixed generic remapping on declarations)
- fix(parser): fixed custom operator parsing [`#fix(parser): fixed custom operator parsing`](https://github.com/plume-lang/plume/issues/fix(parser): fixed custom operator parsing)
- fix(tc): fixed type application constraint solving issue [`#fix(tc): fixed type application constraint solving issue`](https://github.com/plume-lang/plume/issues/fix(tc): fixed type application constraint solving issue)
- fix(parser): fixed custom operator ambiguity [`#fix(parser): fixed custom operator ambiguity`](https://github.com/plume-lang/plume/issues/fix(parser): fixed custom operator ambiguity)
- fix(solving): fixed really nested type extensions constraints solving [`#fix(solving): fixed really nested type extensions constraints solving`](https://github.com/plume-lang/plume/issues/fix(solving): fixed really nested type extensions constraints solving)
- fix(tc): fixed nested type extension predicate solving [`#fix(tc): fixed nested type extension predicate solving`](https://github.com/plume-lang/plume/issues/fix(tc): fixed nested type extension predicate solving)
- fix(tc): fixed locality issue in synthesizing [`#fix(tc): fixed locality issue in synthesizing`](https://github.com/plume-lang/plume/issues/fix(tc): fixed locality issue in synthesizing)
- fix(tc): fixed monadic state operations [`#fix(tc): fixed monadic state operations`](https://github.com/plume-lang/plume/issues/fix(tc): fixed monadic state operations)
- fix(parser): fixed ambiguity with arrow type and closures [`#fix(parser): fixed ambiguity with arrow type and closures`](https://github.com/plume-lang/plume/issues/fix(parser): fixed ambiguity with arrow type and closures)
- fix(tc): added Eq constraint to Plume types [`#fix(tc): added Eq constraint to Plume types`](https://github.com/plume-lang/plume/issues/fix(tc): added Eq constraint to Plume types)
- fix(lexer): fixed indentation parsing functions [`#fix(lexer): fixed indentation parsing functions`](https://github.com/plume-lang/plume/issues/fix(lexer): fixed indentation parsing functions)
- fix(row-poly): removed row polymorphism in Plume [`#fix(row-poly): removed row polymorphism in Plume`](https://github.com/plume-lang/plume/issues/fix(row-poly): removed row polymorphism in Plume)
- fix(translation): fixed error not throwing [`#fix(translation): fixed error not throwing`](https://github.com/plume-lang/plume/issues/fix(translation): fixed error not throwing)
- fix(pretty): fixed annotation pretty printing [`#fix(pretty): fixed annotation pretty printing`](https://github.com/plume-lang/plume/issues/fix(pretty): fixed annotation pretty printing)
- fix(temp): throwing default implementation error on require resolution [`#fix(temp): throwing default implementation error on require resolution`](https://github.com/plume-lang/plume/issues/fix(temp): throwing default implementation error on require resolution)
- fix(parser): parsing now reset IORef at end [`#fix(parser): parsing now reset IORef at end`](https://github.com/plume-lang/plume/issues/fix(parser): parsing now reset IORef at end)
- fix(readme): fixed sentence meaning in readme [`#fix(readme): fixed sentence meaning in readme`](https://github.com/plume-lang/plume/issues/fix(readme): fixed sentence meaning in readme)
- fix(pretty): fixed record pretty printing [`#fix(pretty): fixed record pretty printing`](https://github.com/plume-lang/plume/issues/fix(pretty): fixed record pretty printing)
- fix(typo): fixed typography issue in readme [`#fix(typo): fixed typography issue in readme`](https://github.com/plume-lang/plume/issues/fix(typo): fixed typography issue in readme)
- fix(parser): fixed boolean literal parsing missing [`#fix(parser): fixed boolean literal parsing missing`](https://github.com/plume-lang/plume/issues/fix(parser): fixed boolean literal parsing missing)
- fix(parser): fixed not operator precedence [`#fix(parser): fixed not operator precedence`](https://github.com/plume-lang/plume/issues/fix(parser): fixed not operator precedence)
- fix(parser): removed non-sense empty row type [`#fix(parser): removed non-sense empty row type`](https://github.com/plume-lang/plume/issues/fix(parser): removed non-sense empty row type)
- fix(parser): fixed postfix operators (recsel, recrem, funcall) [`#fix(parser): fixed postfix operators (recsel, recrem, funcall)`](https://github.com/plume-lang/plume/issues/fix(parser): fixed postfix operators (recsel, recrem, funcall))

### Commits

- **Breaking change:** feat(plume): completely rewrote Plume typechecker [`a76d17b`](https://github.com/plume-lang/plume/commit/a76d17b960c132031296422f1c570df79182d977)
- **Breaking change:** feat(compiler): added a MVP bytecode assembler [`8019a8b`](https://github.com/plume-lang/plume/commit/8019a8bfc4565d6b0de4db98d9f85b488dd06206)
- **Breaking change:** feat(require): added Require expressions support [`72d0fc1`](https://github.com/plume-lang/plume/commit/72d0fc1148e763157283d3cfb6f2e3b2c7fabb4b)
- **Breaking change:** feat(macro): implemented macro expansion [`2f0124f`](https://github.com/plume-lang/plume/commit/2f0124fbd6b379c77d462cfba6698e7246eaa3a6)
- **Breaking change:** feat(plume): introduced user defined datatypes [`8e64629`](https://github.com/plume-lang/plume/commit/8e646293150bec711a8fe7512d5039640d9de5ef)
- **Breaking change:** feat(tc): added basic type extension typechecking [`59b5b6b`](https://github.com/plume-lang/plume/commit/59b5b6b78915c1f2875276004af066a2c04716c3)
- **Breaking change:** feat(tc): added typechecker prototype without extensions [`f50702a`](https://github.com/plume-lang/plume/commit/f50702aea45abecf2614e2260d6e823e3224d059)
- **Breaking change:** feat(tc): added new monadic operations for typechecking [`165b8f2`](https://github.com/plume-lang/plume/commit/165b8f2679e4650a23d8c2b847ffb369c6e1b32f)
- **Breaking change:** feat(parser): added expression parser [`af100e7`](https://github.com/plume-lang/plume/commit/af100e786cc53f77d6f906b743723b4d1ac333b5)
- **Breaking change:** feat(std): refined standard library with fresh new datatypes [`0b36671`](https://github.com/plume-lang/plume/commit/0b366719a0b1817d7890ed8f50062264a283d650)
- **Breaking change:** feat(translation): improved error handling [`c5421af`](https://github.com/plume-lang/plume/commit/c5421af168c9c001b70932456ffa207f3790e9ae)
- **Breaking change:** feat(tc): upgraded typechecker extension system to the next level [`f5a9b87`](https://github.com/plume-lang/plume/commit/f5a9b87f677d736f4404e8cd94bd4230a1843571)
- **Breaking change:** feat(parser): added some basic lexing and indentation functions [`27e7c33`](https://github.com/plume-lang/plume/commit/27e7c331310aae6eb7b1e0de38d1051d5a2bec9c)
- **Breaking change:** feat(pretty): generalized record prettyprinting [`6075717`](https://github.com/plume-lang/plume/commit/607571753a212c91b1846655e11706529e65f571)
- **Breaking change:** feat(compiler-cc): added basic untyped closure conversion algorithm [`fbc5b96`](https://github.com/plume-lang/plume/commit/fbc5b96fa56f5022f933654f1b4da308778f8457)
- **Breaking change:** feat(tc): added even more powerful constraint solving on extensions [`4a19d92`](https://github.com/plume-lang/plume/commit/4a19d929841f2d7a3f718527a3a611c677276bf1)
- **Breaking change:** feat(syntax): added custom operator syntax to Plume [`5250792`](https://github.com/plume-lang/plume/commit/5250792d4f51315d4176769d78ba8da2db467a47)
- **Breaking change:** feat(plume): introduced full FFI system in Plume [`72342d7`](https://github.com/plume-lang/plume/commit/72342d7519f589b5a3bd4ee726d3f5699f9dac50)
- **Breaking change:** feat(syntax): added generic property declaration [`796d444`](https://github.com/plume-lang/plume/commit/796d44499fff6fbb3f88bfd4134e8fe7aeaec1a8)
- **Breaking change:** feat(parser): made indentation parser clearer [`07404c6`](https://github.com/plume-lang/plume/commit/07404c62e3fc4cbb1c5e64d75b63e8bdf756d1dc)
- **Breaking change:** feat(vm): added deserializing module [`fd4e9f6`](https://github.com/plume-lang/plume/commit/fd4e9f68f2e780ca0df3aa37db18c9239ea8f996)
- **Breaking change:** feat(typechecker): working on typechecker monad [`96d3011`](https://github.com/plume-lang/plume/commit/96d30114b76785ae503b0a0a79073476f0868978)
- **Breaking change:** feat(compiler): added fully working extension system [`e31807c`](https://github.com/plume-lang/plume/commit/e31807cb5a765609534cef69dcb210645833523c)
- **Breaking change:** feat(parser): introducing string interpolation [`ef0539b`](https://github.com/plume-lang/plume/commit/ef0539bf11ae21b538b11ed365ae2536d48d0d59)
- **Breaking change:** feat(compiler): added SSA conversion to Plume [`fde8b88`](https://github.com/plume-lang/plume/commit/fde8b88aa04a650031c67d52243e7ed0c2cd9add)
- **Breaking change:** feat(cst): added fancy description of expressions [`dbb4ff6`](https://github.com/plume-lang/plume/commit/dbb4ff6cb8d06461f84693af097b72a3aa18bcf6)
- **Breaking change:** feat(plume): added list and slice pattern matching [`169b0d3`](https://github.com/plume-lang/plume/commit/169b0d3a2255e726dc2984abf18ecede1d47fa0d)
- **Breaking change:** feat(parser): added return statement [`a0d0c09`](https://github.com/plume-lang/plume/commit/a0d0c09f6c0d882f9ed768211ebe1210128009c8)
- **Breaking change:** feat(parser): added type extension parsing [`b761b60`](https://github.com/plume-lang/plume/commit/b761b6061bb3ce3612780fd56f1e8d9efec0a616)
- **Breaking change:** feat(std): added list module [`fd7bc20`](https://github.com/plume-lang/plume/commit/fd7bc20b40e7ec57bd8083cdb7e90a1089abe5fd)
- **Breaking change:** feat(parser): added concrete type parsing functions [`0298b5f`](https://github.com/plume-lang/plume/commit/0298b5f0ee02473f936cf75ca0c9c92893bd752d)
- **Breaking change:** feat(compiler-cc): added free and substitute functions on CC IR [`f6ce22d`](https://github.com/plume-lang/plume/commit/f6ce22dacf94d8828c28077ff19d8341c69d583f)
- **Breaking change:** feat(parser): updated record type syntax [`870ade3`](https://github.com/plume-lang/plume/commit/870ade3898937e4b25dbba74125c6597d0865aee)
- **Breaking change:** feat(parser): updated row extension syntax in a more javascriptish way [`10a042c`](https://github.com/plume-lang/plume/commit/10a042cbaa9df75674982ba94c14b5ca03fe8787)
- **Breaking change:** feat(desugar): added ANF and switch conversion [`1cb56cd`](https://github.com/plume-lang/plume/commit/1cb56cdda7857e4c74898f7d0e6153ccd510200a)
- **Breaking change:** feat(prettyprint): added TLIR prettyprinting [`7deddee`](https://github.com/plume-lang/plume/commit/7deddee6e53a107f2cde3ea94433d2d37ce859a6)
- **Breaking change:** feat(desugar): added desugared syntax [`f4de7cd`](https://github.com/plume-lang/plume/commit/f4de7cdef389e179e4f10c2d69a2bd45347bcf74)
- **Breaking change:** feat(tc): added constraint solving for extensions [`21f53d3`](https://github.com/plume-lang/plume/commit/21f53d361afeec9f3af708b85d7d55fa20ae170b)
- **Breaking change:** feat(syntax): added native function declaration [`06a74a2`](https://github.com/plume-lang/plume/commit/06a74a21b55fe7b5f10fdd478afd129db15504a6)
- **Breaking change:** feat(std): added basic string module [`614332a`](https://github.com/plume-lang/plume/commit/614332aefa822f7940b889f46c2c90d54cece1f3)
- **Breaking change:** feat(translation): added operator translation [`87a6766`](https://github.com/plume-lang/plume/commit/87a67662d1322fa7e5faa1522138d5c094547463)
- **Breaking change:** feat(compiler): added bytecode serializing for VM [`806e485`](https://github.com/plume-lang/plume/commit/806e48565fd9e928bae7fe334881cf8c12211a6c)
- **Breaking change:** feat(desugar): added desugaring algorithm [`c02746a`](https://github.com/plume-lang/plume/commit/c02746a6613800a1dfd4f969ea29aa9684e10e2c)
- **Breaking change:** feat(type-erasure): introduced dynamic dispatch codebase [`ffccd98`](https://github.com/plume-lang/plume/commit/ffccd9845062f033c6174cc41a219ba3527f272c)
- **Breaking change:** feat(vm): added bytecode module types [`288c6bd`](https://github.com/plume-lang/plume/commit/288c6bd5a7548a48e7ffe7b23da3879b50143c05)
- **Breaking change:** feat(parser): now parsing correctly pattern matching [`23b7081`](https://github.com/plume-lang/plume/commit/23b7081f29754c48ca8b827291796910b618f914)
- **Breaking change:** feat(pretty): added AST prettyprinting [`a8a1d3c`](https://github.com/plume-lang/plume/commit/a8a1d3c360b06f8912e03928d4ae202e6fd28cff)
- **Breaking change:** feat(std): added math and prelude modules [`da32780`](https://github.com/plume-lang/plume/commit/da3278004a4199e2f9ecc48052aef981cb7b5c79)
- **Breaking change:** feat(tc): dissociating generics and qualified types [`44a0b0e`](https://github.com/plume-lang/plume/commit/44a0b0e7d46d2ad8173dbf61578f4ab4c80f03c8)
- **Breaking change:** feat(typechecker): added Typed Language Intermediate Representation [`902ef35`](https://github.com/plume-lang/plume/commit/902ef350f82b62d9a0de909aa1f49c7d54e65ffe)
- **Breaking change:** feat(parser): added operators description [`25afa30`](https://github.com/plume-lang/plume/commit/25afa308b2a425e5c8875922d03a746778096e7f)
- **Breaking change:** feat(parser): added literal parsing functions [`06bca7a`](https://github.com/plume-lang/plume/commit/06bca7a8afb6ba8fa3f22e5ee5d3594e893cf1f7)
- **Breaking change:** feat(translation): added custom error support [`42c7c77`](https://github.com/plume-lang/plume/commit/42c7c770b54b422358665d874684b552571fcefb)
- **Breaking change:** feat(compiler): added bytecode syntax [`1dc92dc`](https://github.com/plume-lang/plume/commit/1dc92dcea48dca63e06e058f33a036f7038a67bd)
- **Breaking change:** feat(concrete): added module require expression [`9dc0c09`](https://github.com/plume-lang/plume/commit/9dc0c0903c65befc3cca92530ea403d9973fe4ad)
- **Breaking change:** feat(translation): implemented UFCS in translation pass [`54fe9da`](https://github.com/plume-lang/plume/commit/54fe9dab8c808dcf71b6b3ae7bd2074b3325b750)
- **Breaking change:** feat(cst): added concrete types and their aliases [`d5fa36d`](https://github.com/plume-lang/plume/commit/d5fa36dc2ffe7da57d7168d5f2a9c4533ed9c9a5)
- **Breaking change:** feat(standard): added few standard IO functions [`a56acf4`](https://github.com/plume-lang/plume/commit/a56acf4b5833485037d29930b2e08f7055997b21)
- **Breaking change:** feat(record): added automatic HasField macro derivation [`2f8aab3`](https://github.com/plume-lang/plume/commit/2f8aab333239b1b934469ddc94a9cd5fc059c520)
- **Breaking change:** feat(tc): added custom fancy errors [`db0652d`](https://github.com/plume-lang/plume/commit/db0652d8b5d330aa10d553a2d2a7894144010b91)
- **Breaking change:** feat(type-erasure): added type erasure algorithm [`e31037e`](https://github.com/plume-lang/plume/commit/e31037ee86272cda1464ef971bfb90844964b20f)
- **Breaking change:** feat(abstract): added AST type definition [`9f72f24`](https://github.com/plume-lang/plume/commit/9f72f2461bfa63348c7a3d1edd70e807f408a2cb)
- **Breaking change:** feat(std): added some number list methods [`30ddccd`](https://github.com/plume-lang/plume/commit/30ddccd10120c558457bca09a85d5ae09c6a98bd)
- **Breaking change:** feat(compiler-cc): added closure conversion IR definition [`7e0bed3`](https://github.com/plume-lang/plume/commit/7e0bed31a84d439b0bf590b1c88c9df1f2775ca9)
- **Breaking change:** feat(tc): added type unification [`3aadfdc`](https://github.com/plume-lang/plume/commit/3aadfdcd82e50630a8c6f1f519e5c1113617112a)
- **Breaking change:** feat(std): added get_args function [`6008600`](https://github.com/plume-lang/plume/commit/6008600615a5e28b4a228f78e20234f7ef7c6798)
- **Breaking change:** feat(exception): implemented catchIO function [`4ab262d`](https://github.com/plume-lang/plume/commit/4ab262d0de1e7b74f51603e4e8f069634330830a)
- **Breaking change:** feat(type-erasure): added type erased syntax [`41c52f7`](https://github.com/plume-lang/plume/commit/41c52f71d4e0d6df1a38b354eb5567bce3185f99)
- **Breaking change:** feat(translation): added native function translation [`bf56b50`](https://github.com/plume-lang/plume/commit/bf56b5001de7f6f4a3bda87abf710b14ba2d6db5)
- **Breaking change:** feat(translation): added pattern matching translation step [`9f51611`](https://github.com/plume-lang/plume/commit/9f51611e5b7a7996951afba65be8fa7517c68c8f)
- **Breaking change:** feat(vm): added main VM entry [`3df727d`](https://github.com/plume-lang/plume/commit/3df727df03788c77770bdeb32fb33d6df5b1cd9b)
- **Breaking change:** feat(tc): added HasField instances for Checker state [`7daa339`](https://github.com/plume-lang/plume/commit/7daa3399fc7de19c5674a67fea7024968d761693)
- **Breaking change:** feat(tc): added search function for checker state [`fbfa7db`](https://github.com/plume-lang/plume/commit/fbfa7db852ac543ec494ebc49d868beceb6d4ed1)
- **Breaking change:** feat(tc): added local function [`7bb2a79`](https://github.com/plume-lang/plume/commit/7bb2a7933890e2e6b811ff3ff4c4833e5872731b)
- **Breaking change:** feat(parser): added require expression parsing [`574fd77`](https://github.com/plume-lang/plume/commit/574fd771a85db6be35303f51205ea904189b54f2)
- **Breaking change:** feat(syntax): updated abstract and translation modules [`8b3bdda`](https://github.com/plume-lang/plume/commit/8b3bdda29c36ea5ab78c82b1c31d8eccb8067e6c)
- **Breaking change:** feat(expression): added pattern matching [`f0e9d64`](https://github.com/plume-lang/plume/commit/f0e9d643cd83292ea76b6889fa971f3fdd721159)
- **Breaking change:** feat(std): added some system functions [`1a409bb`](https://github.com/plume-lang/plume/commit/1a409bb9923931dd1e9e37f9c45ce3a14bf07699)
- **Breaking change:** feat(vm): added value definition [`29ea7b3`](https://github.com/plume-lang/plume/commit/29ea7b3352ad06e80b2a96b844956c88eff74589)
- **Breaking change:** feat(tc): added generic property verification for extensions [`0f521da`](https://github.com/plume-lang/plume/commit/0f521da996f1ebffabc28789a0f8275b08f69abc)
- **Breaking change:** feat(cst): added type annotation datatype [`47957cf`](https://github.com/plume-lang/plume/commit/47957cf00558926850bdd1ee98563d424a556e01)
- **Breaking change:** feat(syntax): added generics support for function definition [`c9b1c57`](https://github.com/plume-lang/plume/commit/c9b1c573659d27529573b9f99cdc71d1f9ecc97e)
- **Breaking change:** feat(tc): added type conversion function [`3697a13`](https://github.com/plume-lang/plume/commit/3697a13df512dd42de38538dc7578a1b26308d0c)
- **Breaking change:** feat(cst): added CST primitives [`926c8d8`](https://github.com/plume-lang/plume/commit/926c8d8470f7c63c6a55d2327244466da3cbe7f3)
- **Breaking change:** feat(tc): added new specific extension declaration node [`1eb2e31`](https://github.com/plume-lang/plume/commit/1eb2e312d2533439d74e90c7bb204f45db72fa1d)
- **Breaking change:** feat(monad/IO): added IOReader type utility [`595892c`](https://github.com/plume-lang/plume/commit/595892ce45d1a76dcccab0b6a6e68d2b4611ea3e)
- **Breaking change:** feat(parser): added main Parser module entry [`62a61bd`](https://github.com/plume-lang/plume/commit/62a61bd866ab1cb677a35527ee4c9c7e0f803150)
- **Breaking change:** feat(parser): removed typevar parsing [`3d59d29`](https://github.com/plume-lang/plume/commit/3d59d290d3aac2fc913a54b43df33d9762e8c444)
- **Breaking change:** feat(tlir): added type extension TLIR node [`c8b1851`](https://github.com/plume-lang/plume/commit/c8b1851b32fea8dad2bf65717bc9cb9b7ed9afca)
- **Breaking change:** feat(tc): added constraint definition [`5a1976a`](https://github.com/plume-lang/plume/commit/5a1976a3f7c344d9e980bf979a4e4a634a0acfd7)
- **Breaking change:** feat(translation/require): added require translation beginning code [`9a139d9`](https://github.com/plume-lang/plume/commit/9a139d9ef57b8e995dc2fb203e10d19fb1d004ee)
- **Breaking change:** feat(tc): added constraint solver [`9fa6346`](https://github.com/plume-lang/plume/commit/9fa634615ef97704ed4298443357aeb4baf35a8d)
- **Breaking change:** feat(deps): now using relude instead of default prelude [`576c685`](https://github.com/plume-lang/plume/commit/576c68589252788b85fadd198466473118566bcd)
- **Breaking change:** feat(record): removed GHC.Records in favor of native one [`48b0ccf`](https://github.com/plume-lang/plume/commit/48b0ccf5f205fd0250eb12a3f2d3cdc63f124539)
- **Breaking change:** feat(log): added bunch of IO functions [`0e37f92`](https://github.com/plume-lang/plume/commit/0e37f928911398749efabc67d663913f3f37de5d)
- **Breaking change:** feat(records): re-added records overlapping due to lack of features [`49be57a`](https://github.com/plume-lang/plume/commit/49be57aa92f697f511b7f8c19082302d7dfff626)
- **Breaking change:** feat(std): added some IO macros [`ea39a73`](https://github.com/plume-lang/plume/commit/ea39a737f2d60003e98af30e34be6d2390b78c35)
- **Breaking change:** feat(syntax): added list support [`e991c29`](https://github.com/plume-lang/plume/commit/e991c29d49b6264a0b0182332c1fb515d9f694ed)
- **Breaking change:** feat(pretty): added some prettyprinting instances [`ccdb627`](https://github.com/plume-lang/plume/commit/ccdb627f0c3e9c997a5ed600b83e888545a28ba8)
- **Breaking change:** feat(cst): added main CST module entry [`d750cb5`](https://github.com/plume-lang/plume/commit/d750cb5243252b39feca3586fde2789c8ee023f9)
- **Breaking change:** feat(tc): added some utility functions [`47b659a`](https://github.com/plume-lang/plume/commit/47b659ab7a50f93de2d46d353935f9e709308a76)
- **Breaking change:** feat(tc): added main typechecker runner function [`50512e9`](https://github.com/plume-lang/plume/commit/50512e91bdd402f327c376d5faba46b97e249fa7)
- **Breaking change:** feat(tc): added some state related functions [`4f08e96`](https://github.com/plume-lang/plume/commit/4f08e965147ce3012b56919a8a2bf5b175838fa9)
- **Breaking change:** feat(constraints): added extension constraint [`9b78fc6`](https://github.com/plume-lang/plume/commit/9b78fc6df78d29aacfb30dd913e6d6e9cbd014a5)
- **Breaking change:** feat(abstract): added AST module entry [`1962ed3`](https://github.com/plume-lang/plume/commit/1962ed3b8e5c1702bb8e73d3121e044867653dc5)
- **Breaking change:** feat(core): improved new GHC Records with setField function [`f52b003`](https://github.com/plume-lang/plume/commit/f52b003a3190eb481ea687c0d408f7733f21d7b6)
- **Breaking change:** feat(app): calling parsing functions on main example file [`d611917`](https://github.com/plume-lang/plume/commit/d611917b100591aded64892335487c78240c66c6)
- **Breaking change:** feat(tc): added type generalization function [`c56274c`](https://github.com/plume-lang/plume/commit/c56274cac140760f7bcf92624894db5ff17d0056)
- **Breaking change:** feat(std): specialized print and println to string [`8427fc1`](https://github.com/plume-lang/plume/commit/8427fc1834d7261de9a6f03802d91de6974855ae)
- **Breaking change:** feat(tlir): replaced old generics with new powerful generics [`fce69d0`](https://github.com/plume-lang/plume/commit/fce69d0bfc76eb754799f2fee92299ed292f78f4)
- **Breaking change:** feat(tc): added constraints field to state [`9700ebe`](https://github.com/plume-lang/plume/commit/9700ebed199d8c3c8f80b48574b4d13df598d159)
- **Breaking change:** feat(std): added join string list method [`356bceb`](https://github.com/plume-lang/plume/commit/356bceb9ba5da6ff967dbc4dce2d1d90d361441d)
- **Breaking change:** feat(conversion): added type conversion on maybe [`a59eac0`](https://github.com/plume-lang/plume/commit/a59eac067e07e9faa907b5d109da393e8668ca7f)
- **Breaking change:** feat(IO): added new IORef functions [`9c95c4b`](https://github.com/plume-lang/plume/commit/9c95c4bcaa2badd321cbe36a07ca4a05e0cd2614)
- **Breaking change:** feat(tlir): introduced TypeOf internal expression [`de58f8e`](https://github.com/plume-lang/plume/commit/de58f8eadee51101331d0075c69965cdd3ca5346)
- **Breaking change:** feat(translation): added spreadable mapping function [`cbc6bc7`](https://github.com/plume-lang/plume/commit/cbc6bc70c470cc51b6796ca98ea3a076511832a1)
- **Breaking change:** feat(translation): added Translator type module [`5d06781`](https://github.com/plume-lang/plume/commit/5d0678187b62a41ecf97011b44e2fb78f0fc1a7c)
- **Breaking change:** feat(std): added str show extension method [`ab0b189`](https://github.com/plume-lang/plume/commit/ab0b1898ced6b14159a8909b80bedd6bd9395132)
- **Breaking change:** feat(git): introduced VM submodule [`6f21bac`](https://github.com/plume-lang/plume/commit/6f21bac515e49a82680924439012dd0fdd915f7c)
- **Breaking change:** feat(pretty): added Maybe ANSI Pretty instance [`0e4ffac`](https://github.com/plume-lang/plume/commit/0e4ffacb11c7248370bdaee30d839ac65944d069)
- **Breaking change:** feat(parser): updated row restriction with new brand syntax [`dd3f9a9`](https://github.com/plume-lang/plume/commit/dd3f9a9e7cc44cf0a1c12b8766b04b9891677ac4)
- **Breaking change:** feat(IO): added readerIO runner function [`3994ca4`](https://github.com/plume-lang/plume/commit/3994ca43798decca5c1a863f2fc3b54204898d30)
- **Breaking change:** feat(prettyprint): added prettyprinting for tuples [`24c59bd`](https://github.com/plume-lang/plume/commit/24c59bd46ac186ca2b324fb687a4ffe328395671)
- **Breaking change:** feat(literal): added Ord instance for literals [`479a4aa`](https://github.com/plume-lang/plume/commit/479a4aa04c31b0fbfe52d061f9de3bceb7da7fe4)
- **Breaking change:** feat(tlir): added list support [`3c6113c`](https://github.com/plume-lang/plume/commit/3c6113ce1ad5d2dc2378fbd75782fece192f56fa)
- **Breaking change:** feat(tc): added empty match error [`1b86c54`](https://github.com/plume-lang/plume/commit/1b86c5447b5090d417cac8b20d8b1034aa3ee934)
- **Breaking change:** feat(tc): introducing compiler error [`2a26ba3`](https://github.com/plume-lang/plume/commit/2a26ba3852a7ab6691ad6ce220dbc48a78ea5235)
- **Breaking change:** feat(import): updated imports due to library change. [`c96e30a`](https://github.com/plume-lang/plume/commit/c96e30a7a548fc3f5d906d5e88cbca0e4dcfb5fe)
- **Breaking change:** feat(tlir): added wildcard pattern type [`a6c8b69`](https://github.com/plume-lang/plume/commit/a6c8b69bec67a2ee4a4975d59288b31948f02453)
