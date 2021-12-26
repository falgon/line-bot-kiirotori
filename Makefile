SHELL := /bin/bash
INSTALL_PATH := /usr/local/bin
BIN_NAME := line-bot-kiirotori
NGINX_SITES_AVAILABLE_PATH=/etc/nginx/sites-available
CONF_NAME := api-linebot.roki.dev.conf
SYSTEMD_SERVICE_NAME := $(BIN_NAME).service
EXECUTOR_CONF_DIR := /root/.config/lb-kiirotori

release:
	@stack build --flag line-bot-kiirotori:release --ghc-options -O2

debug:
	@stack build --fast

restyle:
	@find ./app ./src -type f -name "*.hs" | xargs -n1 stylish-haskell -i

restyle-with-hlint: restyle
	hlint ./app ./src

install:
	@stack install \
		--flag line-bot-kiirotori:release \
		--ghc-options -O2
	@sudo mv $(HOME)/.local/bin/$(BIN_NAME) $(INSTALL_PATH)
	@sudo mkdir -p $(EXECUTOR_CONF_DIR) && sudo cp -rv ./docker/redis $(EXECUTOR_CONF_DIR)
	@sudo cp -v .$(NGINX_SITES_AVAILABLE_PATH)/$(CONF_NAME) $(NGINX_SITES_AVAILABLE_PATH)
	@sudo cp -v ./etc/systemd/$(SYSTEMD_SERVICE_NAME) /etc/systemd/system
	@sudo systemctl daemon-reload

uninstall:
	@sudo $(RM) $(INSTALL_PATH)/$(BIN_NAME)
	@sudo systemctl stop $(SYSTEMD_SERVICE_NAME)
	@sudo systemctl disable $(SYSTEMD_SERVICE_NAME)
	@sudo $(RM) -rfv \
		$(NGINX_SITES_AVAILABLE_PATH)/$(CONF_NAME) \
		/etc/systemd/system/$(SYSTEMD_SERVICE_NAME)
	@sudo bash -c 'pushd $(EXECUTOR_CONF_DIR)/redis && docker-compose down --rmi all --volumes && popd'

.PHONY: release debug restyle restyle-with-hlint install uninstall
