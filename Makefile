all: test

compile:
	$(MAKE) -C src

clean:
	rm -rf src/*.beam
	rm -rf tests/*.beam

test:
	$(MAKE) -C src
	$(MAKE) -C tests
