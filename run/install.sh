#!/bin/bash

rsync -av --progress --delete --exclude .git ./ /usr/local/src/automator
chown -R $USER:$USER /usr/local/src/automator
sudo cp ./run/initd /etc/init.d/automator
sudo cp  ./run/default /etc/default/automator

echo "Automator installed to /usr/local/src/automator"
echo "You can configure defaults in /etc/default/automator"
echo "You may control it vi /etc/init.d/automator {start,stop,status}"
echo "It is NOT added to startup"
