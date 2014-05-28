#!/bin/bash

if [ "$#" -ne 1 ];then
    echo "Must include a user argument" 2>&1
    exit 1
fi

if [ "$(id -u)" != "0" ]; then
    echo "This script must be run as root" 2>&1
    exit 1
fi
USER=$1

sudo service automator stop
sudo rsync -av --progress --delete --exclude .git ./ /usr/local/src/automator
sudo chown -R $USER:$USER /usr/local/src/automator
sudo cp ./run/initd /etc/init.d/automator
sudo cp  ./run/default /etc/default/automator
sudo sed -i -e"s/#my_user_here#/$USER/" /etc/default/automator
sudo service automator start

echo "Automator installed to /usr/local/src/automator"
echo "You can configure defaults in /etc/default/automator"
echo "You may control it vi /etc/init.d/automator {start,stop,status}"
echo "It is NOT added to startup"
