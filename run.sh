dist/build/server/server +RTS -N4 -c -H512M &
pid=$!
sleep 2
dist/build/client/client +RTS -N4 -c -A4096k -H512M -sstderr&
dist/build/client/client +RTS -N4 -c -A4096k
#dist/build/client/client +RTS -N4 -I0.1 -A2048k -F2 -qg1 -qb2 -c -G2 -H512M -K16M -xc -B -sstderr
kill -9 $pid
