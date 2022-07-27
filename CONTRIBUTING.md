# Contributing

## Building with Docker

### Prerequisites

This guide assumes you have installed the following development tools for your 
operating system:

- [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
- [docker](https://docs.docker.com/engine/install/)

```bash
git clone https://github.com/tochicool/bitcoin-dca
cd bitcoin-dca
```

### Building and installing the binary


```bash
docker build . --target builder
```

### Running the unit tests

```bash
docker build . --target tester
```

## Building directly on host

### Prerequisites

This guide assumes you have installed the following development tools and 
libraries for your operating system:

- [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
- [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/)
- [libsecp256k1](https://github.com/bitcoin-core/secp256k1)

```bash
git clone https://github.com/tochicool/bitcoin-dca
cd bitcoin-dca
```

### Building and installing the binary

```bash
stack install
```

### Running the unit tests

```bash
stack test
```
