dist/build/server/server &
pid=$!
dist/build/client/client +RTS -N4 -p
kill -9 $pid
