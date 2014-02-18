dist/build/server/server &
pid=$!
sleep 1
dist/build/client/client +RTS -N4 -A1024k -c -G4 -H512M
kill -9 $pid
