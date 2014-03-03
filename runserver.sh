dist/build/server/server +RTS -xt -hy -P &
pid=$!
sleep 5
kill -9 $pid
#dist/build/client/client +RTS -N4 -I0.1 -A2048k -F2 -qg1 -qb2 -c -G2 -H512M -K16M -xc -B -sstderr
