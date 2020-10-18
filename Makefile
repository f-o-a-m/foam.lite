.PHONY: help install build-app build-dapp serve-app test-dapp deploy-contracts run-server cliquebait-start cliquebait-stop cliquebait-logs cliquebait-tail cliquebait-restart
.DEFAULT_GOAL := help

MAKE_PID := $(shell echo $$PPID)
JOBS_FLAG ?= $(subst -j,,$(filter -j%, $(subst -j ,-j,$(subst --jobs ,-j,$(shell ps T | grep "^\s*$(MAKE_PID).*$(MAKE)")))))
JOBS := $(or $(JOBS_SET),$(JOBS_SET),$(shell sh -c 'nproc 2>/dev/null || sysctl -n hw.physicalcpu 2>/dev/null'))

CLIQUEBAIT_CONTAINER_NAME ?= foam5g-cliquebait
CLIQUEBAIT_IMAGE ?= foamspace/cliquebait:latest
CLIQUEBAIT_LOCAL_PORT ?= 8545

NODE_URL ?= http://localhost:$(CLIQUEBAIT_LOCAL_PORT)

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

install: ## Install dependencies
	npm install & wait

build-dapp: install ## Build the DApp (contracts, etc.)
	npm run chanterelle-build
	npm run build-dapp

test-dapp: build-dapp cliquebait-start ## Starts cliquebait if needed and runs the test suite
	npm run test-dapp

cliquebait-start: ## Starts a Cliquebait instance in the background
	@if docker inspect $(CLIQUEBAIT_CONTAINER_NAME) >/dev/null 2>&1; \
	then \
	  echo "Cliquebait with name $(CLIQUEBAIT_CONTAINER_NAME) is already running"; \
	else \
	  docker run --rm -i -t -d -p $(CLIQUEBAIT_LOCAL_PORT):8545 -v $(shell pwd)/cliquebait.json:/cliquebait/cliquebait.json --name $(CLIQUEBAIT_CONTAINER_NAME) $(CLIQUEBAIT_IMAGE) ;\
	fi

cliquebait-stop: ## Stops the Cliquebait instance running in the background, if one is running
	@if docker inspect $(CLIQUEBAIT_CONTAINER_NAME) >/dev/null 2>&1; \
	then \
		docker stop $(CLIQUEBAIT_CONTAINER_NAME); \
	else \
		echo "Background cliquebait with name $(CLIQUEBAIT_CONTAINER_NAME) is not running"; \
	fi

cliquebait-logs: ## Show the logs of a running background Cliquebait instance
	@if docker inspect $(CLIQUEBAIT_CONTAINER_NAME) >/dev/null 2>&1; \
	then \
		docker logs $(CLIQUEBAIT_CONTAINER_NAME); \
	else \
		echo "Background cliquebait with name $(CLIQUEBAIT_CONTAINER_NAME) is not running"; \
	fi

cliquebait-tail: ## Tail the logs of a running background Cliquebait instance
	@if docker inspect $(CLIQUEBAIT_CONTAINER_NAME) >/dev/null 2>&1; \
	then \
		docker logs -f $(CLIQUEBAIT_CONTAINER_NAME); \
	else \
		echo "Background cliquebait with name $(CLIQUEBAIT_CONTAINER_NAME) is not running"; \
	fi

cliquebait-restart: ## Stop and start the background Cliquebait instance
	@$(MAKE) cliquebait-stop
	@$(MAKE) cliquebait-start

build-app: install ## Build the frontend
	npm run build-app

serve-app: ## bundle and serve the app
	npm run bundle-app && npm run browserify && npm run serve

deploy-contracts: build-dapp ## Deploy contracts so the server can run
	npm run deploy-contracts

run-server: ## Run the relayer server
	npm run run-server
