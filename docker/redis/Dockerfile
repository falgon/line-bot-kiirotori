FROM redis:6.2
SHELL ["/bin/bash", "-c"]
ENV TZ=Asia/Tokyo \
    DEBIAN_FRONTEND=noninteractive \
    DEBCONF_NOWARNINGS=yes
COPY --chown=root:root ./start.sh /usr/local/bin/start.sh
COPY --chown=root:root ./set-pin-code.sh /usr/local/bin/set-pin-code.sh
COPY --chown=root:root ./crontab /var/spool/cron/crontabs/root
RUN chmod +x /usr/local/bin/start.sh /usr/local/bin/set-pin-code.sh \
    && apt-get -qq update \
    && apt-get install -qq -y --no-install-recommends busybox-static \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
CMD ["/bin/bash", "-c", "/usr/local/bin/start.sh"]
