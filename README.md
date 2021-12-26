# 🐥 line-bot-kiirotori

## Requirements

- systemd
- nginx
    - Virtual host settings required
- docker

## Install

```bash
cd line-bot-kiirotori
make install
```

## Run

```bash
sudo systemctl start line-bot-kiirotori
```

## Development build and run

```bash
stack build --fast
stack exec line-bot-kiirotori -- serve -c <config path>
```

## Configration

Put the following configuration file in any location (default: `~/.config/lb-kiitoroti/config.toml`).

```toml
[app]
welcome_message = "piyo" # welcome message

[redis]
hostname = "localhost"
port = 6379
password = "" # option
select = 0
max_connections = 50
max_idle_time = 30

[line]
kid = "" # Assertion Signing Key
user_id = "" # user id
channel_id = "" # channel id
channel_secret = "" # channel secret
jwk_set_key = "" # jwk private set key, c.f. https://developers.line.biz/en/docs/messaging-api/generate-json-web-token/#generate-private-and-public-key
```
