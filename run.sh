dist/build/server/server &
pid=$!
dist/build/client/client +RTS -N4 -xc
kill -9 $pid
