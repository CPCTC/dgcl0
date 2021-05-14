SOURCES = $(wildcard src/*.cl)
FASLS = $(addprefix build/,$(notdir $(SOURCES:.cl=.fasl)))
TOP = main
BIN = dgcl0

GLOBAL_FLAGS = --noinform
NON_INTERACTIVE_FLAGS = --noprint --quit --disable-debugger --no-sysinit --no-userinit

override _fasl =

.PHONY: env
env: $(FASLS)
	rlwrap -nH /dev/null sbcl \
		$(GLOBAL_FLAGS) \
		$(foreach _fasl,$^,--load $(_fasl))

$(BIN): build/bin
	ln -sf $^ $@

build/$(BIN): $(FASLS)
	sbcl \
		$(GLOBAL_FLAGS) \
		$(NON_INTERACTIVE_FLAGS) \
		$(foreach _fasl,$^,--load $(_fasl)) \
		--eval '(save-lisp-and-die "$@" :toplevel '\''$(TOP) :executable T)'

$(FASLS): build/%.fasl: src/%.cl | build
	sbcl \
		$(GLOBAL_FLAGS) \
		$(NON_INTERACTIVE_FLAGS) \
		--eval '(multiple-value-bind (path warnp failp) (compile-file "$^" :output-file "../$@") (declare (ignore path)) (if (or warnp failp) (error "Compilation failure.")))' \
	|| { rm -f $@; exit 1; }

build:
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf build $(BIN)
