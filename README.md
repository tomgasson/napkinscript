# napkinscript [![Build Status](https://travis-ci.org/IwanKaramazow/napkinscript.svg?branch=master)](https://travis-ci.org/IwanKaramazow/napkinscript) [![Build status](https://ci.appveyor.com/api/projects/status/a8d1hx0xi17tk14j?svg=true)](https://ci.appveyor.com/project/IwanKaramazow/napkinscript)

## Getting Started
Install napkinscript using [`yarn`](https://yarnpkg.com/en/package/napkinscript):

```bash
yarn add --dev napkinscript
```

Or [`npm`](https://www.npmjs.com/):

```bash
npm install --save-dev napkinscript
```

Update your `bsconfig.json`:
```javascript
  "pp-flags": "napkinscript",
```

## Dev Setup

Required:
- [NodeJS](https://nodejs.org/) 
- Ocaml 4.06.1
- OS: Mac/Linux

```
git clone git@github.com:IwanKaramazow/napkinscript.git
cd napkinscript
npm install

opam switch create 4.06.1
```

## Dev workflow

Run the tests
```
make test
```

Debug a file
```
# edit file.js in the root of the project
make debug
```
