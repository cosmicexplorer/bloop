#!/usr/bin/env zsh

set -euxo pipefail

sbt publish{Local,M2} \
  &> >(tee /dev/stderr) \
  | sed -rne 's#^.*bloop-frontend_2.12-(1.3.2.*)\.pom.*#\1#gp' \
  | head -n1 \
         > ./BLOOP_VERSION

BLOOP_VERSION="$(cat ./BLOOP_VERSION)"

sed -r -i -e "s#(BLOOP_REV_TO_FIND=)(.*$)#\\1'${BLOOP_VERSION}'#g" \
    ./bloop-invoke/blp-server

pushd "$OS_PANTS_SRC"
sed -r -i -e "s#(BLOOP_REV = )(.*$)#\1'${BLOOP_VERSION}'#g" \
    contrib/bloop/3rdparty/jvm/BUILD
popd
