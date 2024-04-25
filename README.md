<br/>
<p align="center">
  <img src="docs/logo.svg" width="128">
  <h3 align="center">The Plume Programming Language</h3>

  <p align="center">
    A programming language that promotes functional programming for everyone.
    <br/>
    <br/>
    <a href="https://github.com/plume-lang/plume/wiki"><strong>Explore the docs »</strong></a>
    <br/>
    <br/>
    <a href="https://github.com/plume-lang/plume/issues">Report Bug</a>
    .
    <a href="https://github.com/plume-lang/plume/issues">Request Feature</a>
    .
    <a href="https://discord.gg/rJGdHEfKhj">Join the community</a>
  </p>
</p>

<div align="center">
  
![Downloads](https://img.shields.io/github/downloads/plume-lang/plume/total) 
![Contributors](https://img.shields.io/github/contributors/plume-lang/plume?color=dark-green) 
![Issues](https://img.shields.io/github/issues/plume-lang/plume) 
![License](https://img.shields.io/github/license/plume-lang/plume)

</div>

## Table Of Contents

- [About the Project](#about-the-project)
- [Built With](#built-with)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [Roadmap](#roadmap)
- [Contributing](#contributing)
- [License](#license)
- [Authors](#authors)

## About The Project

Plume is a programming language that aims to be as simple as existing languages like Python. It has a great learning curve, permitting users either to perform simple tasks or to accomplish large projects.

Its syntax has been designed to be non-aggressive and free, resulting in a pleasant-to-use and very extensible language. Plume supports some very cool features such as:

- **Powerful data structures** enabled with ADTs let you take serious advantage of the language type system
- **Extension system**, allowing you to define generalized behaviors for your types
- **Strict type system**, letting you serenely compile your programs without worrying about runtime errors
- **Safe standard library** to avoid basic but hard-to-find bugs
- **Macro system** to expand actual syntax and to avoid unnecessary function calls

These are some of the language features available with Plume. But it still has other interesting properties to offer to you like platform-independent code or binary (because it either runs on a virtual machine or compiles down to WASM), great runtime performances, and so on..

## Built With

Plume has been built using Haskell and some specific libraries

- [megaparsec](https://hackage.haskell.org/package/megaparsec)
- [mtl](https://hackage.haskell.org/package/mtl)
- [directory](https://hackage.haskell.org/package/directory)
- [filepath](https://hackage.haskell.org/package/filepath)

## Getting Started

This is an example of how you may give instructions on setting up your project locally.
To get a local copy up and running follow these simple example steps.

### Prerequisites

You need to install these dependencies in order to compile and run Plume.

- GHC (`>= 9.8.1`): https://www.haskell.org/get-started/
- Cabal (`>= 3.0`): https://www.haskell.org/cabal/

> You could alternatively have installed GHCup which is an universal dependency manager for Haskell based tools.

### Installation

1. Clone the repo

```sh
git clone https://github.com/plume-lang/plume.git
```

2. Update fetched Cabal packages

```sh
cabal update
```

3. Install or run Plume

```sh
cabal install # or cabal run in order just to run it
```

## Roadmap

See the [open issues](https://github.com/plume-lang/plume/issues) for a list of proposed features (and known issues).

## Contributing

Contributions are what make open-source communities such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

- If you have suggestions for adding or removing projects, feel free to [open an issue](https://github.com/plume-lang/plume/issues/new) to discuss it, or directly create a pull request after you edit the _README.md_ file with the necessary changes.
- Please make sure you check your spelling and grammar.
- Create individual PR for each suggestion.
- Please also read through the [Code Of Conduct](https://github.com/plume-lang/plume/blob/main/CODE_OF_CONDUCT.md) before posting your first idea as well.

### Creating A Pull Request

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

Distributed under the MIT License. See [LICENSE](https://github.com/plume-lang/plume/blob/main/LICENSE.md) for more information.

## Authors

- **Sisypheus** - _Programming language designer_ - [Sisypheus](https://github.com/sisypheus-dev) - _Built most of the language_
