k8s_yaml(kustomize('deploy/kustomize/base-sealed-secret'))
docker_build('ghcr.io/tochicool/bitcoin-dca', '.', target='builder')
k8s_resource('bitcoin-dca')
