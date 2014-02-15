dist/build/server/server &
pid=$!
dist/build/client/client +RTS -N2 -sstderr -p
kill -9 $pid
