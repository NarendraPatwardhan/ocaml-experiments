.DEFAULT_GOAL := help
PROJECT := $(shell basename $(CURDIR))
BOLD := $(shell tput bold)
RESET := $(shell tput sgr0)

.PHONY: setup # Setup the build requirements
setup:
	@echo "${BOLD}Setting up the build image...${RESET}"
	@docker build -t $(PROJECT):builder .
	@echo "${BOLD}Making build directory...${RESET}"
	@mkdir -p build

.PHONY: build # Build the project
build:
	@echo "${BOLD}Building the project...${RESET}"
	@docker run --rm -v ${CURDIR}/main.ml:/app/main.ml \
		-v ${CURDIR}/build:/app/build \
		$(PROJECT):builder \
		ocamlopt -o build/main main.ml

.PHONY: help # Display the help message
help:
	@echo "${BOLD}Available targets:${RESET}"
	@cat Makefile | grep '.PHONY: [a-z\+]' | sed 's/.PHONY: / /g' | sed 's/ #* / - /g'
