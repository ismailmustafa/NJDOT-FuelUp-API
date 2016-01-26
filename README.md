# NJDOT Fuel Up API #

This is the backend for the njdot-fuelup app located [here](https://github.com/jayrav13/njdot-fuelup).
This API was created with [Servant](https://haskell-servant.github.io).

### How to run: ###

First get [The Haskell Tool Stack](http://www.haskellstack.org) if you don't already have it.

```
stack setup 
stack build
stack exec njdot-fuelup-api-exe
```

It will begin listening on listen on 127.0.0.1:80/

This api has two GET requests:

```
GET /bridges?latitude=<lat>&longitude=<lng>
GET /stations?latitude=<lat>&longitude=<lng>
```

The latitude and longitude parameters are required otherwise an empty
list will be returned. Both endpoints return JSON formatted locations
ordered by distance away from the specified coordinates.

### If you want to recreate the database (sqlite3): ###

```
rm njdot-fuelup.db
stack ghci
Main Api Database Model> initializeDB
```

### To run tests: ###

```
stack test
```

