#!/usr/bin/env fish

exec java -cp /home/protected/server.jar App \
  beerdledash.nfshost.com \
  8080 \
  https://beerdledash.com/
