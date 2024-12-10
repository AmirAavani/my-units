run: build
	./Sample01

build:
	mkdir -p /tmp/Sample01
	fpc -Fu../.. -Fu../../../General-Purpose-Units -Fu../../../ALogger -Fu../../../Unicode-Functions -Fu../../../Threading -Fu../../../PB2PAS -Fu../../../Algorithmic-Units -Fu../../Protos Sample01.lpr  -FU/tmp/Sample01/

clean:
	rm -Rf /tmp/Sample01/

