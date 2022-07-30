# Bitcoin - Dollar Cost Averaging

[![CI/CD](https://github.com/tochicool/bitcoin-dca/actions/workflows/cicd.yml/badge.svg)](https://github.com/tochicool/bitcoin-dca/actions/workflows/cicd.yml)

Service which automates dollar cost averaging with exchange APIs with support
for:

* Multiple asset pairs
* Auto withdrawals
* Output descriptors
* Order size control

and more.

## Downloads
You can download binaries from the latest [releases](https://github.com/tochicool/bitcoin-dca/releases/tag/latest).

## Usage
The program reads its configuration from an external yaml configuration file:


```
bitcoin-dca - Automate dollar cost averaging on exchanges

Usage: bitcoin-dca [-f|--config-file CONFIG]
  Validate and run the strategies as configured from CONFIG

Available options:
  -f,--config-file CONFIG  The path to the config yaml file to load
                           (default: "config/config.yaml")
  -h,--help                Show this help text
```


### Example: Recurring buys and auto withdraw

```yaml
version: 2.0.0
exchanges:
  coinbase:
    baseUrl: https://api-public.sandbox.exchange.coinbase.com
    accessKey: 6efb3cc87f19dad41921adc0a9c06d06
    passphrase: 8edd3hm2ops
    secretKey: 7eA2w7uyI2rx28qLzo/13cC3jWtS6qalBn0bpGtv/4IM/NXbtm63Gc94yfVvnGF/CztCm8EAlVp/YgAi5TiFKw==
    strategies:
    - type: ScheduledBuys
      buy: BTC
      withFunds: 20 USD
      frequency: '@weekly'
      minimumAmount: 1 USD
    - type: SweepWithdraw
      withdraw: BTC
      address: bc1qh5xf4vz8zlpptljr2rh8yrxwh4yu4va2m9hy7q
```

### Example: Multiple asset pairs

```yaml
version: 2.0.0
exchanges:
  exchangeName:
    ...
    strategies:
    - type: ScheduledBuys
      buy: BTC
      withFunds: 20 USD
      frequency: '@weekly'
      minimumAmount: 1 USD
    - type: ScheduledBuys
      buy: BTC
      withFunds: 20 GBP
      frequency: '@weekly'
      minimumAmount: 1.99 GBP
    - type: ScheduledBuys
      buy: XBT
      withFunds: 100 EUR
      frequency: '@monthly'
      minimumAmount: 5 EUR
    ...
```


### Example: Single-use withdraw addresses (with Output Descriptors)

> **Warning**: Support for output descriptors in the project is experimental and
 for advanced users only.

```yaml
version: 2.0.0
exchanges:
  exchangeName:
    ...
    strategies:
    ...
    - type: SweepWithdraw
      withdraw: BTC
      outputDescriptor: wpkh(xpub68UvhVKMH5JnHcJKWXV59oQGgrRddUG863eEa7QZeuTQCxJV9EshCm1uatYJqxiCox2RmhJyfrmhCMNi1bmfU67d2ghuQ3qw4uZFLFJMLgs/0/*)#z0mq7vhf

```

### Full documentation

The full configuration documentation is given below, with some examples:

```yaml
# (Required) The SemVer version of the config.
version: 2.0.0
# (Required) Exchange configuration.
exchanges:

  # (Default: {}) Coinbase API configuration.
  coinbase:

    # (Default: https://api.exchange.coinbase) The base URL for API requests.
    baseUrl: https://api-public.sandbox.exchange.coinbase.com # Public sandbox environment

    # (Documentation: https://docs.cloud.coinbase.com/exchange/docs/authorization-and-authentication#generating-an-api-key)
    accessKey: 6efb3cc87f19dad41921adc0a9c06d06
    passphrase: 8edd3hm2ops
    secretKey: 7eA2w7uyI2rx28qLzo/13cC3jWtS6qalBn0bpGtv/4IM/NXbtm63Gc94yfVvnGF/CztCm8EAlVp/YgAi5TiFKw==

    # (Default: []) A list of strategies to run on against exchange.
    strategies:
    - # (Required) The type of the strategy instance.
      type: ScheduledBuys # Automatic recurring buys

      # (Required) The asset to buy.
      buy: BTC

      # (Required) The funds to buy the asset with.
      withFunds: 20 USD

      # (Required) A schedule for when the funds are supplied, as a cron 
      # expression or one of @yearly, @monthly, @weekly, @daily or @hourly.
      frequency: '@weekly'

      # (Default: 0) The minimum amount of funds to use on a single order. This
      # constraint will only be applied as permitted by the exchange. If 
      # specified, the denomination must be equal that used in 'withFunds'.
      minimumAmount: 10 USD

    - # (Required) The type of the strategy instance
      #   SweepWithdraw - Note that the amount this strategy can withdraw is
      #     limited by the amounts purchased by other strategies running in the 
      #     same process.
      type: SweepWithdraw

      # (Required) The asset to withdraw
      withdraw: BTC

      # (One of address or outputDescriptor required) The address to withdraw 
      # the asset to. The address format is only validated by the program for 
      # Bitcoin withdraws.
      address: bc1qh5xf4vz8zlpptljr2rh8yrxwh4yu4va2m9hy7q
      # (One of address or outputDescriptor required) The output descriptor to
      # use to derive fresh withdraw addresses. This option is only valid for 
      # Bitcoin withdraws. The strategy will try to avoid reusing the derived 
      # addresses by querying the configured explorer. The descriptor should be 
      # ranged, use standard scripts, and not be a Pay-To-PublicKey script. 
      # The strategy may terminate if it cannot derive any more unused addresses
      # from the output descriptor.
      # See BIP-380 for more info on output descriptors: https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki
      outputDescriptor: wpkh(xpub68UvhVKMH5JnHcJKWXV59oQGgrRddUG863eEa7QZeuTQCxJV9EshCm1uatYJqxiCox2RmhJyfrmhCMNi1bmfU67d2ghuQ3qw4uZFLFJMLgs/0/*)#z0mq7vhf

      # (Default: 0) The minimum size of withdraws, useful for controlling dust.
      # This constraint will only be applied if permitted by the exchange. 
      # If specified, the denomination must be equal 'withdraw'.
      minimumSize: 0.000010000 BTC

      # (Default: 0) The minimum funds that must be spent for a withdraw to be
      # initiated. This constraint will only be applied if permitted by the 
      # exchange. If specified, the denomination must be used as funds by 
      # another strategy running on the same exchange in this process.
      minimumFunds: 5 USD

# (Default: {}) Explorer configuration
explorers:
  # (Default: {}) Esplora API configuration
  esplora: 
    # (Default: https://blockstream.info:443/api) The base URL for API requests.
    baseUrl: http://localhost:8080/api # Your local esplora API server

```

## Build

See build instructions on the [contribution guide](CONTRIBUTING.md).


## Deployment

The project includes example configuration for a GitOps deployment workflow to a Kubernetes using either sealed secrets or a plaintext `config.yaml`.

### Kubernetes with Github Secrets
With appropriate access control on your repo, you can check in Kubernetes
secret manifest with the config:

1. Fork this repository.
2. Go to forked repo -> settings -> secrets -> actions and add the following key
value pairs:
    * **KUBE_CONFIG**: Kubernetes configuration file with authentication details for your cluster.
    * **CONFIG**: The contents of a config.yaml file which configures this 
    application instance.
3. Replace base resource in [deploy/kustomize/production/kustomization.yaml](deploy/kustomize/production/kustomization.yaml)
with `../base-secret`.
3. Go to forked repo -> actions and enable the workflow.

This will trigger the pipeline and deploy the config changes.

### Kubernetes with Sealed secrets
1. Fork this repository.
2. Go to forked repo -> settings -> secrets -> actions and add the following key
value pairs:
    * **KUBE_CONFIG**: Kubernetes configuration file with authentication details for your cluster.

   Ensure that the **CONFIG** secret is not set in your repo.

3. Go to forked repo -> actions and enable the workflow.
4. Check in your `config.yaml` as a sealed secret. For example if `secrets/config-secret.yaml`
contains:

```yaml
apiVersion: v1
kind: Secret
type: Opaque
metadata:
  name: config
  annotations:
    sealedsecrets.bitnami.com/managed: "true"
    sealedsecrets.bitnami.com/namespace-wide: "true"
stringData:
  config.yaml: |
    version: 2.0.0
    exchanges:
      coinbase:
        baseUrl: https://api-public.sandbox.exchange.coinbase.com
        accessKey: 6efb3cc87f19dad41921adc0a9c06d06
        passphrase: 8edd3hm2ops
        secretKey: 7eA2w7uyI2rx28qLzo/13cC3jWtS6qalBn0bpGtv/4IM/NXbtm63Gc94yfVvnGF/CztCm8EAlVp/YgAi5TiFKw==
        strategies:
        - type: ScheduledBuys
          buy: BTC
          withFunds: 20 USD
          frequency: '@weekly'
          minimumAmount: 1 USD
        - type: SweepWithdraw
          withdraw: BTC
          address: bc1qh5xf4vz8zlpptljr2rh8yrxwh4yu4va2m9hy7q
```
you can check in the sealed secret with:
```bash
git clone https://github.com/<your-username>/bitcoin-dca.git
cd bitcoin-dca
kubeseal --controller-namespace bitcoin-dca -n bitcoin-dca -o yaml <secrets/config-secret.yaml >deploy/kustomize/base-sealed-secret/config-sealed-secret.yaml
d-secret.yaml
git commit -am "feat(config): updated config"
git push
```
This will trigger the pipeline and deploy the config changes.

## License

This project is licensed under the BSD-3-Clause License - see the [LICENSE](LICENSE) file for details.
