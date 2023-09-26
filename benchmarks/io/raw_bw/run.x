mpirun -np 6 ../../../bin/raw_bw.x --n_streams 1 --nx 4 --n_levs 1 --n_packets 1 | head -1
for ns in 1 2 3 6; do
    for nx in 180 360 720 1440; do
    	for n_levs in 91 137; do
	   	mpirun -np 6 ../../../bin/raw_bw.x --n_streams $ns --nx $nx --n_levs $n_levs --n_packets 1 | tail -1
	done
    done
done
