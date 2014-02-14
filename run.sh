dist/build/server/server &
pid=$!
dist/build/client/client +RTS -N4 -p -sstderr -H512m
kill -9 $pid
