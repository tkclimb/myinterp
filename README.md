# Myinterp
Simple ML-like language interpreter.

# Prerequisite
All you need is just build the docker container prepared.
```shell
docker build -t myinterp:latest {myinterp-repository-directory}
```

# Usage
Run your prepared docker container.

```shell
Go to {myinterp-repository-directory}
docker run -it --rm  myinterp:latest
```

You can take an expression by pipe like below.

```shell
echo '3 * (1 + 2)' | dune exec myinterp
# (* 3 (+ 1 2)) = 9
```