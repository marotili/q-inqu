dist/build/server/server &
pid=$!
dist/build/client/client +RTS -N4
kill -9 $pid
