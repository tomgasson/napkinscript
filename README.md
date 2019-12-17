# napkinscript [![Build Status](https://travis-ci.org/IwanKaramazow/napkinscript.svg?branch=master)](https://travis-ci.org/IwanKaramazow/napkinscript) [![Build status](https://ci.appveyor.com/api/projects/status/a8d1hx0xi17tk14j?svg=true)](https://ci.appveyor.com/project/IwanKaramazow/napkinscript)

## Getting Started
#### Install
Install napkinscript using [`yarn`](https://yarnpkg.com/en/package/napkinscript):

```bash
yarn add https://github.com/IwanKaramazow/napkinscript
```

Or [`npm`](https://www.npmjs.com/):

```bash
npm install https://github.com/IwanKaramazow/napkinscript
```

#### bs-platform
* Make sure bs-platform is version `6.0.1` or above.
* Update your `bsconfig.json`:
```javascript
  "pp-flags": "napkinscript/lib/napkinscript.exe",
```

#### Editor (vscode)
* use reason-vscode extension 1.7.1
* make sure ocaml vscode extension is disabled
* If you run reason-vscode master, set the config `Reason_language_server: Mlfmt` for the experimental formatter
```
/full/path/to/napkinscript.exe -print ns
```

## Dev Setup

Required:
- [NodeJS](https://nodejs.org/) 
- Ocaml 4.06.1
- OS: Mac

```
git clone git@github.com:IwanKaramazow/napkinscript.git
cd napkinscript
npm install

opam switch create 4.06.1
```

## Dev workflow

#### Run the tests
```
make test
```

#### Debug a file
```
# write code in test.js
make build # build bytecode for fast dev iteration
./lib/napkinscript.exe -print ns test.js # test printer
./lib/napkinscript.exe -print ast test.js # print ast
./lib/napkinscript.exe -print ml test.js # show ocaml code
```

#### benchmark
```
make build-native
./lib/napkinscript.exe -bench
```
