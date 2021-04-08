#!/usr/bin/env fish

echo "Assembling..."
and mill app.assembly

and echo "Preparing deploy..."
and mkdir -vp deploy/public deploy/protected
and rsync -v out/app/assembly/dest/out.jar deploy/protected/server.jar
and rsync -av --del --exclude .DS_Store scripts deploy/protected
and rsync -av --del --exclude .DS_Store static deploy/public

and echo "Done."
