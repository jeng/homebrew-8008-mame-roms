ASL=/usr/local/bin/asl
P2BIN=/usr/local/bin/p2bin
P2HEX=/usr/local/bin/p2hex

hellorld: hellorld.asm
	$(ASL) -cpu 8008 -L hellorld.asm -o hellorld.p
	$(P2BIN) hellorld.p hellorld.bin

hexdec: hexdec.asm
	$(ASL) -cpu 8008 -L hexdec.asm -o hexdec.p
	$(P2BIN) hexdec.p hexdec.bin


primes: primes.asm
	$(ASL) -cpu 8008 -L primes.asm -o primes.p
	$(P2BIN) primes.p primes.bin

prime-master: prime-master.asm
	$(ASL) -cpu 8008 -L prime-master.asm -o prime-master.p
	$(P2BIN) prime-master.p prime-master.bin
