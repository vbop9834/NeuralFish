test:
	$(info Running tests...)
	sh build.sh test

watch:
	$(info Watching Tests...)
	sh watchAndRunTests.sh

build:
	$(info Building...)
	sh build.sh build
