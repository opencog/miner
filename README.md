# Miner

opencog | singnet
------- | -------
[![CircleCI](https://circleci.com/gh/opencog/miner.svg?style=svg)](https://circleci.com/gh/opencog/miner) | [![CircleCI](https://circleci.com/gh/singnet/miner.svg?style=svg)](https://circleci.com/gh/singnet/miner)

The miner (or pattern miner) is a frequent and surprise subhypergraph
pattern miner for the AtomSpace. It is built on top of the URE to take
advantage of the URE refined control capabilities.

## Building and Installing

### Prerequisites

To build the miner you need to build and install the
[URE](https://wiki.opencog.org/w/URE) first, see
[Building-and-installing-the-URE](https://github.com/opencog/ure#building-and-installing)
for more information.

### Building Miner

Be sure to install the pre-requisites first!
Perform the following steps at the shell prompt:
```
    cd miner
    mkdir build
    cd build
    cmake ..
    make -j
```
Libraries will be built into subdirectories within build, mirroring
the structure of the source directory root.

### Unit tests

To build and run the unit tests, from the `./build` directory enter
(after building opencog as above):
```
    make -j test
```
Tests can be run in parallel as well:
```
    make -j check ARGS=-j4
```

### Install

After building, you must install the pattern miner.
```
    sudo make install
```

## Examples

Examples can be found in this repository under

[Miner examples](examples/miner)

## More info

The primary documentation for the pattern miner is here:

* [Pattern Miner wiki](https://wiki.opencog.org/w/Pattern_miner)
* [Pattern Miner README.md](opencog/miner/README.md)
