dist/build/server/server &
pid=$!
sleep 1
dist/build/client/client +RTS -N4 &
dist/build/client/client +RTS -N4
#dist/build/client/client +RTS -N4 -I0.1 -A2048k -F2 -qg1 -qb2 -c -G2 -H512M -K16M -xc -B -sstderr
kill -9 $pid
