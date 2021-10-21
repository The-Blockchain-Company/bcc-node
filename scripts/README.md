# Scripts Overview

The `scripts` directory consists of the following directories:
- [benchmarking](#benchmarking)
- [buildkite](#buildkite)
- [cole-sophie-evie-jen](#cole-sophie-evie-jen)
- [lite](#lite)
- [sophie-from-scratch](#sophie-from-scratch)
- [cole-to-aurum](#cole-to-aurum)
- [zerepoch](#zerepoch)

#### benchmarking
Contains all the scripts relevant to benchmarking `bcc-node`. See the benchmarking [README](benchmarking/README.md).

#### buildkite
Contains scripts relevant to BCIO's CI.

#### cole-sophie-evie-jen
Contains a script that sets up a cluster beginning in the Cole era and can transition to the Sophie era. You can also start a cluster in the Sophie, Evie or Jen era by supplying an argument to `mk-files.sh`.
E.g
```bash
./scripts/cole-to-jen/mk-files.sh sophie # Starts nodes in Sophie era
./scripts/cole-to-jen/mk-files.sh evie # Starts nodes in Evie era
./scripts/cole-to-jen/mk-files.sh jen    # Starts nodes in Jen era
```
#### lite
Contains scripts that can start various clusters and intended to be as simple as possible. Note that using the sophie only era testnet clusters breaks compatibility with some cli commands.

#### sophie-from-scratch
Contains a script that creates all the necessary keys etc to create a sophie cluster from scratch.

#### cole-to-aurum
Contains a script that creates all the necessary keys and configuration files to create an aurum cluster from scratch.
This script is similar to [cole-sophie-evie-jen](#cole-sophie-evie-jen) script: It can either be used to start cluster in Cole and then gradually transition to Aurum, or can jumpstart straight into selected era, eg.:
```
./scripts/cole-to-aurum/mkfiles.sh aurum
```
will start the cluster in Aurum era from epoch 0.

#### zerepoch

Contains scripts to test submission of transactions containing (simple) Zerepoch scripts. Obviously, only works against a test cluster running in Aurum era.

Example:
```
$ scripts/zerepoch/example-txin-locking-zerepoch-script.sh guessinggame
```
will post several transactions validating against the simple guessing Game zerepoch contract.
