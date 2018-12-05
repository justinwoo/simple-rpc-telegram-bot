default: install build

run:
	node .

nix:
	nix-shell --run make

install:
	npm install
	nix-shell install-deps.nix --run "echo installation complete"

purs-deps:
	psc-package2nix

build:
	purp bundle
