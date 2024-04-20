# parlelm

an implementation of mastermind with 5 letter words on a constrained vocabulary with a UI
that might be inspired to something mentioned in this [article](https://www.404media.co/nytimes-files-copyright-takedowns-against-hundreds-of-wordle-clones/).

goal is to have in production version that can replace this (with changes in the UI, e.g. color changes) https://github.com/pietroppeter/wordle-it

we still have to complete the MVP, these are some 
ideas for additional features
- play a past game
- history of all your past games
- share a custom game

Started at [![Open Source Saturday](https://img.shields.io/badge/%E2%9D%A4%EF%B8%8F-open%20source%20saturday-F64060.svg)](https://www.meetup.com/it-IT/Open-Source-Saturday-Milano/)
Shared project by @pietroppeter (beginner in Elm and functional languages) and @akiross (code ninja that wants to put more Elm in production).

We also tried (on the first meetup) to have a NixOS based Elm setup with help by @kristoff-it but it did not work out.
In the end it is actually very simple to just setup elm with the installer.

## tests

using elm-explorations/test and node-test-runner
- install using `npm install -g elm-test`
- test using `elm-test src/Word.elm`
