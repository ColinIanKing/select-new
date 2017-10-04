# Select Drivers
*select-drivers* is a tool which ease the process of porting kernel drivers from
one version to another.
It enables the automatic collection of drivers files, from a time period or commits
range, and the automatic classification of porting compilation errors.

## Requirement
 * ocaml 4.02+
 * python3 3.5+
 * prequel

## Installation
### External dependencies
*select-drivers* relies on external git repositories, which are listed as submodules.
Before compiling this software, you need to update the submodules:
```bash
git submodule update --init
```
This will clone and pull the necessary dependencies.

### Compilation
To compile *select-drivers* execute the `make` command:
```bash
make
```

### Setting up the templates
During execution *select-drivers* will generate `Makefile`s from a template
which need to be personalized.
To personalize your template run the `setup.py` script:
```bash
./setup.py
```
Be sure to have a working prequel installation and
an up-to-date prequel indices database before running this script.

If you change your prequel installation in the future you can run this script
again to update your template.


## Running *select-drivers*
In order to run *select-drivers*, a working git repository of the linux kernel
must be present on your device.
You also need to provide either a commit list, a commit range or a date range
for the automatic drivers collection.

Here a simple example:
```bash
./select_drivers --range v4.8..v4.9 --target v4.12 4.12_porting ../linux
```
This example will collect all introduced drivers from version 4.8 to version 4.9
and will try to port them to version 4.12, using the `../linux` linux git repository.
The results of the execution will be stored in the `4.12_porting` directory.

### Multi-threaded execution
Execution of *select-drivers* can be quite long due to the necessary compilation
of drivers.
It is possible to parallelize the execution with the `--cores` argument.
In order to do so **you must duplicate your linux git repository for each thread**.
If you have N threads and your linux git repository is named `linux` you need
to create N repository named `linux0` to `linux(N-1)`
