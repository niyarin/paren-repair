all:
	mkdir -p bin
	csc -X r7rs -R r7rs -static cli.scm -o bin/paren-repair


.PHONY: test
test:
	./bin/paren-repair test-resources/n.scm
