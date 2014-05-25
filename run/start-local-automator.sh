export PIPE_DIR=/var/tmp/run

LOGDIR=./rel/automator/log
LOGFILE=automator-sandbox.log
ABSLOGPATH=`readlink -f $LOGDIR/$LOGFILE`

COOKIE=it

echo 'stopping...'
./rel/automator/bin/automator stop \
    -sname sandbox \
    -setcookie $COOKIE

echo 'starting...'
./rel/automator/bin/automator start \
    +W w +P 100100 \
    -setcookie $COOKIE \
    -s automator \
    -kernel error_logger '{file,"'$ABSLOGPATH'"}' \
    -boot start_sasl \
    -sname sandbox
