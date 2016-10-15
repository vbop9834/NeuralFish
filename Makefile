test:
	$(info Running tests...)
	sh build.sh test

watchtests:
	$(info ...)
	sh watchAndRunTests.sh

build:
	$(info Building...)
	sh build.sh build
