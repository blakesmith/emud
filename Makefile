all: test

compile:
	$(MAKE) -C src

clean:
	rm -rf ebin/*.beam

test:
	$(MAKE) -C src
	$(MAKE) -C tests
