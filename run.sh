dist/build/server/server +RTS &
pid=$!
sleep 1
dist/build/client/client +RTS -N4 -c -A4096k -xc &
pid3=$!
#dist/build/client/client +RTS -N4 -I0.1 -A2048k -F2 -qg1 -qb2 -c -G2 -H512M -K16M -xc -B -sstderr
sleep 30
kill -9 $pid3
kill -9 $pid
