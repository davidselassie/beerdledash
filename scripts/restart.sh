#!/usr/bin/env fish

nfsn stop-daemon server
and echo "Server stopped."
and nfsn start-daemon server
and echo "Server started."
