# Pattern Mining MOZI-AI KBs

Experiments running the pattern miner over the various gene ontologies
and other KBs from MOZI-AI.

## Usage

### Install bioscience

First you need to install `agi-bio`, go to some directory where you
put source code and clone the `agi-bio` repository

```bash
git clone https://github.com/opencog/agi-bio.git
cd agi-bio
mkdir build
cd build
cmake ..
make -j4
sudo make install
sudo ldconfig /usr/local/lib/opencog
```

A `bioscience` module containing atom type definitions such as
`GeneNode` should now be installed on your system.

### Import KBs

Go back to this directory, where that example is, and type the
following commands:

```bash
mkdir kbs
wget -r --no-parent https://mozi.ai/datasets/
mv mozi.ai/datasets/* kbs
trash mozi.ai
trash kbs/index.html
```

Note: you may replace `trash` by `rm -fr`, but don't forget that with
power comes responsability.

Now you should have scheme files under the `kbs` directory.

### Run Pattern Miner

```bash
guile --no-auto-compile -l mine-mozi-ai.scm
```

which will run the pattern miner of the files. The results will be
logged in the `opencog.log` file at the lines

```
Results from mining <FILENAME>:
```

### Tweaking Parameters

Go in `mine-mozi-ai.scm`, the run-mozi-ai-miner function accepts a
list of parameters that can be changed for each KB.
