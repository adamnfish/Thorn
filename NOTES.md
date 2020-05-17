Notes
=====

## Goals

+ implement API calls
    + newRound
    + placeDisc
    + bid
    + pass
    + flip
    + ping
+ Build CLI client
+ Build web frontend
+ CDK / Cloudformation for API and webapp
+ Deploy pipeline
+ Get art assets for plants / thorns
+ E2E integration testing
+ Extend game functionality
  (powers, betting, etc)


## TODOS

+ Add pool of discs for players and enforce this in the place logic
+ Attempt traverse should collect failures
+ Consider how to disconnect players?



## Rosebud addon

Bet rosebuds on the outcome of each round. Final score is:

    (round victories x rosebuds)

Think about where rosebuds go from / to when won and lost.

Some options:

* bet 1/0 with each disc place, on there not being a Thorn in your stack
  - lose them if a Thorn is revealed, double them if not
* bet at the end of placing, when bidding is opened
  * bet at the end of bidding
  - on whether it will be successful?
  - on fail / success?
  - on where a Thorn will be hit?
