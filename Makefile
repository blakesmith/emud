all: compile

compile:
	$(MAKE) -C src

clean:
	cd src; rm -rf *.beam

test:
	$(MAKE) -C src
	$(MAKE) -C tests
