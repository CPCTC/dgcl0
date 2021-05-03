SOURCES = $(wildcard src/*.cl)
FASLS = $(addprefix build/,$(notdir $(SOURCES:.cl=.fasl)))
TOP = one
BIN = bin

override _fasl =

.PHONY: env
env: $(FASLS)
	rlwrap -nH /dev/null sbcl --noinform $(foreach _fasl,$^,--load $(_fasl))

$(BIN): build/bin
	ln -sf $^ $@

build/$(BIN): $(FASLS)
	echo -n | sbcl --noinform --noprint $(foreach _fasl,$^,--load $(_fasl)) --eval '(save-lisp-and-die "$@" :toplevel '\''$(TOP) :executable T)'

$(FASLS): build/%.fasl: src/%.cl | build
	echo -n | sbcl --noinform --noprint --eval '(compile-file "$^" :output-file "../$@")'

build:
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf build $(BIN)
