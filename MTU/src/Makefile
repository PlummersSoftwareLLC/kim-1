all: mtuGFX/lib/mtuGFX.lib kimDemo/kimDemo.bin kimHello/kimHello.bin kimLife/kimLife.bin kimTest/kimTest.bin kimSieve/kimSieve.bin
	echo "All done."

mtuGFX/lib/mtuGFX.lib:
	cd mtuGFX && make 

kimDemo/kimDemo.bin: mtuGFX/lib
	cd kimDemo && make

kimLife/kimLife.bin: mtuGFX/lib
	cd kimLife && make

kimSieve/kimSieve.bin: mtuGFX/lib
	cd kimSieve && make

kimTest/kimTest.bin: mtuGFX/lib
	cd kimTest && make

kimHello/kimHello.bin: mtuGFX/lib
	cd kimHello && make

clean: 
	cd mtuGFX && make clean
	cd kimDemo && make clean
	cd kimLife && make clean
	cd kimSieve && make clean
	cd kimTest && make clean	
	cd kimHello && make clean
