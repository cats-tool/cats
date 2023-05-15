# CATS - Causal Analysis on Temporal Sequences

This repository contains CATS - a fully-automatic causality checker in reactive systems.
To clone this repo and **initialize the submodules** run the following

``` 
git clone https://github.com/cats-tool/cats
cd cats
git submodule init
git submodule update
```

This README contains instructions on how to build CATS from sources. 
For evaluation, you do **not** need to build CATS yourself; we provide Docker files.
Details on how to build the images and reproduce the results are given in `Reproduce_Experiments.md`.

## Structure 

- `src/` contains the source code of CATS (written in F#). 
- `app/` is the target folder for the build. The final CATS executable will be placed here.
- `examples/` contains example cause checking instances
- `docker/` contains various files needed to construct a Docker container of CATS 
- `eval/` contains additional source code that is used to evaluate CATS


## Build CATS

### Dependencies

We require the following dependencies:

- [.NET 7 SDK](https://dotnet.microsoft.com/en-us/download) (tested with version 7.0.203)
- [spot](https://spot.lrde.epita.fr/) (tested with version 2.11.5)

Install the .NET 7 SDK (see [here](https://dotnet.microsoft.com/en-us/download) for details).
Download and build spot (details can be found [here](https://spot.lrde.epita.fr/)). 
You can install/build spot in any location of your choosing. 
CATS requires the *absolute* path to spot (see details below).


### Build CATS

To build CATS run the following (when in the main directory of this `tool` folder).

```shell
cd src/CATS
dotnet build -c "release" -o ../../app
cd ../..
```

Afterward, the CATS executable (called `CATS`) is located in the `app/` folder.


### Connect spot to CATS

CATS uses the spot executables *autfilt* and *randaut*.
CATS is designed such that it only needs the **absolute** path to these executables, so they can be installed and placed at whatever locations fits best.
The absolute paths are specified in a `paths.json` configuration file. 
This file must be located in the *same* directory as the CATS executables (this convention makes it easy to find the config file, independent of the relative path CATS is called from). 
We already provide a template file `app/paths.json` that *needs to be modified*. 
After having built spot and CATS, paste the absolute path to the *autfilt* and *ltl2tgba* executables to the `paths.json` file. 
For example, if `/usr/bin/autfilt` and `/usr/bin/randaut` are the absolute paths to the *autfilt* and *randaut*, respectively, the content of `app/paths.json` should be

```json
{
    "autfilt":"/usr/bin/autfilt",
    "randaut":"/usr/bin/randaut"
}
```

## Run CATS

After having built CATS and connected spot, you are ready for causality anayslsi. 
The two basic input modes are *cause checking* and *cause sketching*.
For the following, we assume that the current directory is the main tool folder. 

### Cause Checking

To check a cause candidate you can run 

```shell
app/CATS --check <path>
```
where `<path>` is the path to the checking instance.

For example run
```shell
app/CATS --check examples/intro_example.txt
```
to check the cause verification problem in `examples/intro_example.txt`. 
The format itself should be self-explanatory. 
We provide details in the paper.
Note that the system can be given as any automation in the HANOI automaton format. 

### Cause Sketching

To sketch a cause, i.e., explore multiple candidates,  you can run 

```shell
app/CATS --sketch <path>
```

where `<path>` is the path to the sketching instance.
The sketch format itself is similar to that for a checking instance. 
The only difference is that the `[sketch]` field now can contain holes of the form `?{"<AP1>", "<AP2>", ..., "<APn>"}`, where <AP1>, <AP2>, ..., <APn> are APs (note that they must be escaped).
This will explore all boolean combinations (at the moment only clauses) of the APs given in the set.
