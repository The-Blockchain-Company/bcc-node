# Building Bcc Node with nix

The [Nix Package Manager][nix] can be installed on most Linux distributions by downloading and
running the installation script:
```
curl -L https://nixos.org/nix/install > install-nix.sh
chmod +x install-nix.sh
./install-nix.sh
```
and following the directions.

#### TBCO Binary Cache

To improve build speed, it is possible to set up a binary cache maintained by TBCO (**this is
optional**):
```
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee /etc/nix/nix.conf
substituters = https://cache.nixos.org https://hydra.tbco.io
trusted-public-keys = tbco.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.tbco.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF
```

Once Nix is installed, log out and then log back in then:
```
git clone https://github.com/The-Blockchain-Company/bcc-node
cd bcc-node
nix-build -A scripts.mainnet.node -o mainnet-node-local
./mainnet-node-local/bin/bcc-node-mainnet
```

[nix]: https://nixos.org/nix/
