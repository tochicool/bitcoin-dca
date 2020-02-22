# CoinBase Pro - Dollar Cost Averaging

![CI/CD](https://github.com/Tochicool/coinbase-pro-dca/workflows/CI/CD/badge.svg)

Service which automates dollar cost averaging with the CoinBase Pro API

## Downloads
You can download binaries from the latest [releases](https://github.com/Tochicool/coinbase-pro-dca/releases/tag/latest).

## Usage
The program reads its configuration from an external yaml configuration file. For example:

```yaml
accessKey: '7e0adde7b8f03fb0d3589397eaa2ff0f'
secretKey: 'HsbmHun7dHVUnGSis8+dGEGFenLn9y6qEYfIfVgUwa281CfSCZ5jreLsWcdSbIqFOUYYTHT88OaVzuRwlO32XA=='
passphrase: '19cvof5bylgi'
amount: 10
productId: BTC-USD
frequency: '@weekly'
sandbox: True
```
Most of these can be overwritten by command line arguments: 

```
Available options:
  -c,--config-file CONFIG  The path to the configuration file. For security
                           reasons, the API secret key must be set
                           here. (default: "config.yaml")
  -k,--access-key ACCESS_KEY
                           The API access key
  -p,--access-passphrase,--passphrase PASSPHASE
                           The API access key
  -i,--product-id PRODUCT_ID
                           The ID of the product to DCA into
  -a,--amount AMOUNT       The target amount of the quote currency to spend on
                           the product every trade
  -f,--frequency FREQUENCY How often to DCA into the product. Accepts a cron
                           expression or one of @yearly, @monthly, @weekly,
                           @daily or @hourly
  -s,--sandboxed           Use the sandboxed development API environment
  -q,--quiet               Sets the log level to NOTICE and above
  -h,--help                Show this help text
```

## Build

 These are the instructions if you wish to build the binaries yourself, for development or testing purposes.

### Prerequisites

You will require the latest stable [git](https://git-scm.com/downloads) and [haskell stack](https://docs.haskellstack.org/en/stable/README/) binaries for your build system.

### Installing

Clone the repository and its submodules

```bash
git clone --recurse-submodules https://github.com/Tochicool/coinbase-pro-dca
```

Build and install binaries

```bash
cd coinbase-pro-dca
stack install
```

You should now have the coinbase-pro-dca executable in your path:
```bash
coinbase-pro-dca --help
```

### Running the tests

```bash
stack test
```

## Deployment

You can use this projects CI/CD github workflow to deploy the service to your own remote ubuntu server as follows:

1. Fork this repository
2. Go to forked repo -> settings -> secrets and add the following key value pairs:
    * CONFIG: The contents of a config.yaml file which configures your application instance
    * SSHKEY: The contents of the private key of an sshkey pair that has been copied to the target server
    * USERNAME: The username with root permissions to login as. Most likely 'root'
    * HOST: The hostname of the server
    * PORT: The ssh port
3. Go to forked repo -> actions and enable the workflow

## License

This project is licensed under the BSD-3-Clause License - see the [LICENSE](LICENSE) file for details
