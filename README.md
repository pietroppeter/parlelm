# wordlelm

a wordle clone in elm - WIP

goal is to have a production full featured clone of wordle that can be used to replace https://github.com/pietroppeter/wordle-it

after the current full feature set of wordle we will think about adding features, e.g.:
- play a past game
- history of all your past games
- share a custom game

I am a beginner in both Elm and functional languages.

Started at [![Open Source Saturday](https://img.shields.io/badge/%E2%9D%A4%EF%B8%8F-open%20source%20saturday-F64060.svg)](https://www.meetup.com/it-IT/Open-Source-Saturday-Milano/)

Help on Elm by @akiross.

We also tried to have a NixOS based Elm setup with help by @kristoff-it but it did not work out.
In the end it is actually very simple to just setup elm with the installer.

## plan

- [ ] view (with elm-ui)
  - [x] basic layout (grid and keyboard)
  - [x] color the words
- [ ] logic of game and updates
- [ ] side effects (local storage, current date)


## tests

using elm-explorations/test and node-test-runner
- install using `npm install -g elm-test`
- test using `elm-test src/Word.elm`