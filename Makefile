CONTAINER=docker
BUILD_FLAGS ?=

all: bin

bin: clean
	sbcl $(BUILD_FLAGS) --load build.lisp

release: clean
	mkdir -p release
	$(CONTAINER) build -f ./docker/Dockerfile \
		--no-cache \
		--target binary \
		--output type=local,dest=release .

clean:
	-rm -f *.fasl screen-realtor
	-rm -fr release

.PHONY: bin run release clean
