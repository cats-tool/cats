# Reproduce Experiments from the Paper

This file contains instructions on how to reproduce the evaluation results for CATS.
To do so, you do *not* need to build CATS yourself; everything will be installed in a separate Docker image. 
Make sure you have installed the following:

- [Docker](https://www.docker.com/) (tested with version 20.10.24)

Make sure the Docker daemon is started!

## Table 1

To reproduce Table 1, we build CATS in a Docker image. 
When in the main directory of this repository, run 

```shell
docker build -t cats -f docker/Dockerfile_cats .
```
Note that building the images takes about 10-20min.

Afterwards, you should see the `cats` image when running `docker images`.


You can use 
```
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/<file>
```
where `<file>` is the file containing the cause-checking instance. 
Note that this file *must* be located in the `examples/` folder. 

To test that the Docker image works as intended, run 
```
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/example_1.txt
```

### Evaluation

When everything works as expected, you can run all instances in Table 1 (in the same order) as follows:

```shell
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/atva22_arbiter_spurious.txt
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/atva22_arbiter_simple.txt
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/atva22_arbiter.txt
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/example_1.txt
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/example_1_mod.txt
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/example_3_odd.txt
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/example_3_globally.txt
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/example_5.txt
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/example_5_mod.txt
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/tp_left.txt
docker run --mount src="$(pwd)/examples",target="/home/examples",type=bind cats --check examples/tp_right.txt
```


## Figure 2

The data displayed in Figure 2 is computed by an evaluation tools which internally uses CATS.
The source code is located in `eval/EvalSyntcomp`.
We provide a Docker file that construct a Docker image that produces the data in Figure 2.
Build this image by running 

```shell
docker build -t evals -f eval/Dockerfile_eval_syntcomp .
```
Afterwards, you should see the `evals` image when running `docker images`.

### Evaluation

Reproduce the data in Figure 2 by running
```shell
docker run evals
```
This will compute the data in Figure 2 and output all points (i.e., pairs of size and time) in a JSON format to the console. 
Plotting the data (as done in Figure 2) is then straightforward using any tool of your choosing. 


## Table 2

The data displayed in Table 2 is also computed by an evaluation tools which internally uses CATS.
The source code is located in `eval/EvalSketchRandom`.
We provide a Docker file that construct a Docker image that produces the data in Figure 2.
Build this image by running 

```shell
docker build -t evalr -f eval/Dockerfile_eval_sketch_random .
```
Afterwards, you should see the `evalr` image when running `docker images`.

### Evaluation

Reproduce the data in Table 2 by running
```shell
docker run evalr
```
This will compute the data in Table 2 and print this data for each system size. 
