EMACS ?= emacs
PKG = generate
LOAD_PATH  += -L .
LOAD_PATH  += -L ./test

.PHONY: test-primitives test-runner test-org

test-primitives: ## Run primitives tests
test-primitives: 
	$(EMACS) --batch -L . \
		 -L ./test \
		 -l generate-primitives-tests.el \
		 --eval "(ert-run-tests-batch-and-exit)";

test-runner: ## Run test-runner tests
test-runner: 
	$(EMACS) --batch -L . \
		 -L ./test \
		 -l generate-test-runner-tests.el \
		 --eval "(ert-run-tests-batch-and-exit)";

test-org: ## Run org-mode tests
test-org: 
	$(EMACS) --batch -L . \
		 -L ./test \
		 -l generate-org-mode-tests.el \
		 --eval "(ert-run-tests-batch-and-exit)";

scratch: 
	$(EMACS) --batch -L . \
		 -L ./test \
		 -l scratch.el \






