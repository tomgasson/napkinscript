## Valparaiso [![Build Status](https://travis-ci.org/IwanKaramazow/Valparaiso.svg?branch=master)](https://travis-ci.org/IwanKaramazow/Valparaiso)

## Setup

Required:
- [NodeJS](https://nodejs.org/) 
- Ocaml 4.06.1
- OS: Mac/Linux

```
git clone git@github.com:IwanKaramazow/Valparaiso.git
cd Valparaiso
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
