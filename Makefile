EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -l scripts/build.el

.PHONY: all build translate compile test test-extensions klambda clean

all: build

build: translate compile

# klambda/*.kl -> compiled/*.el
translate:
	$(BATCH) -f shen-elisp-build-translate

# Byte-compile (and native-compile unless SHEN_ELISP_NATIVE=0).
compile:
	$(BATCH) -f shen-elisp-build-compile

# Kernel test suite from the kernel/ submodule.
test:
	cd kernel/tests && yes n | ../../bin/shen-elisp eval -l runme.shen

test-extensions:
	cd kernel && yes n | ../bin/shen-elisp eval -l tests/extensions/runme.shen

# Regenerate klambda/ from kernel/ sources: make klambda SHEN=/path/to/shen
klambda:
	scripts/make-klambda.sh $(SHEN)

clean:
	rm -rf compiled eln src/*.elc
