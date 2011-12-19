default: test

test: clean_environment
	guile -L . --no-auto-compile test/combinator.scm

clean_environment:
	rm -f combinators.log

.PHONY: test clean_environment
