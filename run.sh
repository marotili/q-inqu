dist/build/server/server &
pid=$!
dist/build/client/client
kill -9 $pid
