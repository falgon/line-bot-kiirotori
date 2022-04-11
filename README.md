# 🐥 line-bot-kiirotori [![test](https://github.com/falgon/line-bot-kiirotori/actions/workflows/test.yml/badge.svg)](https://github.com/falgon/line-bot-kiirotori/actions/workflows/test.yml)

LINE Bot for personal use that I use with [Messaging API](https://developers.line.biz/ja/services/messaging-api/).

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
stack exec line-bot-kiirotori -- serve -c <config path> -s <cron path>
```

## Test

```bash
stack test --fast
```

## Configration

Put the following configuration file in any location (default: `~/.config/lb-kiitoroti/config.toml`, `~/.config/lb-kiirotori/schedule.cron`).

```toml
[app]
welcome_message = "piyo" # welcome message
during_auth = ""
success_auth = ""
failed_auth = ""
already_auth = "the channel name %s is authorized at %s"
unknown_cmd_message = "🤔"
port = 48080 # port number

[mysql]
hostname = "" # host name, e.g. "127.0.0.2"
port = 3306 # port number
password = ""
username = ""
database = ""
charset = "" # charset, e.g. 224 (utf8mb4)

[redis]
hostname = "" # host name, e.g. "127.0.0.2"
port = 6379 # port number
password = ""
select = 0
max_connections = 50
max_idle_time = 30

[line]
kid = "" # assertion signing key
user_id = "" # user id
channel_id = "" # channel id
channel_secret = "" # channel secret
channel_name = "" # channel name
jwk_set_key = "" # jwk private set key, c.f. https://developers.line.biz/en/docs/messaging-api/generate-json-web-token/#generate-private-and-public-key
```

```cron
# Send a push message to the
# user UXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX every minute, "hello!"
* * * * * UXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX push-text-message hello!
```
