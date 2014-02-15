dist/build/server/server &
pid=$!
sleep 1
dist/build/client/client
kill -9 $pid
