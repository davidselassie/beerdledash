#!/usr/bin/env fish

set HOST nfs_bdd

echo "Rysncing..."
and rsync -avz --progress --del --exclude .DS_Store deploy/* $HOST:/home/

and echo "Restarting..."
and ssh $HOST /home/protected/scripts/restart.sh

and echo "Done."
