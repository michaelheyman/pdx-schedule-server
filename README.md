# pdx-schedule-server

This project replaces the backend implementation of [pdx-schedule](https://github.com/michaelheyman/pdx-schedule/) and reimplements it in Haskell.

A sample client and database are included with the project in `dist/resources/`, but these can be generated with the [pdx-schedule](https://github.com/michaelheyman/pdx-schedule/) project.

The server has endpoints at `/classes` and at root, which deliver the database query and the client, respectively.

## Build

```
cabal new-build
```
 
or

```
stack build
```

## Run

```
cabal new-run
```

or

```
stack run
```
