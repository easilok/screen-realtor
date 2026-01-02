BUILD_FLAGS ?=

all: bin

bin: clean
	sbcl $(BUILD_FLAGS) --load build.lisp

clean:
	rm -f *.fasl lp-displayer

.PHONY: bin run clean
