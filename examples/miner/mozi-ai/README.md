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

Now go back to that directory.

### Import More KBs

The mozi-ai example is provided with a small dataset. If you wish to
use that small sample dataset, first decrompress it

```bash
xz -d kbs/mozi-ai-sample.scm.xz
```

Once decompressed you may directly jump to the Run Pattern Miner
Section below. Otherwise, if you wish to use larger datasets provided
by mozi-ai, assuming you have a beefy machine (last gen CPU with at
least 16GB of RAM) at your disposal, read on.

From this directory, where that example is, type the following
commands:

```bash
wget -r --no-parent https://mozi.ai/datasets/
mv mozi.ai/datasets/* kbs
trash mozi.ai
trash kbs/index.html
cat kbs/*.scm > kbs/all.scm
```

Note: you may replace `trash` by `rm -r`, but don't forget! With great
power comes great responsibility.

Now you should have multiple scheme files under the `kbs` directory.

### Run Pattern Miner

By default the demo uses the provided dataset `mozi-ai-sample.scm`
under the folder `kbs`. If you wish to use another dataset go to
`mine-mozi-ai.scm` and modify the variable `kb-filename`
accordingly. Then enter the following

```bash
guile --no-auto-compile -l mine-mozi-ai.scm
```

which will run the pattern miner over the provided dataset. The
results will be logged in a `opencog*.log` file, after the line

```
Results from mining <kb-filename>:
```

as well as stored in `results` variable accessible from guile after
mining.

### Tweaking Parameters

Go to `mine-mozi-ai.scm` and change the parameters under the line

```scheme
;; Set parameters
```
