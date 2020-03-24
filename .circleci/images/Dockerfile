FROM circleci/clojure:lein-2.7.1

# Verify the circleci user exists before proceeding
RUN whoami

# Installations command expect to run as root
USER root

# Setup google chrome
RUN set -ex \
    && wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add - \
    && echo "deb http://dl.google.com/linux/chrome/deb/ stable main" | tee /etc/apt/sources.list.d/google-chrome.list \
    && apt update \
    && apt install -y google-chrome-stable

# Setup npm
RUN set -ex \
    && apt install -y curl \
    && curl -sL https://deb.nodesource.com/setup_10.x | bash - \
    && apt install -y build-essential nodejs \
    && npm i -g shadow-cljs karma-cli

USER circleci
