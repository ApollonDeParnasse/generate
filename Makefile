EMACS ?= emacs
PKG = generate
LOAD_PATH  += -L .
LOAD_PATH  += -L ./tests

.PHONY: test-primitives test-runner test-org check

test-primitives: ## Run primitives tests
test-primitives: 
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l generate-primitives-tests.el \
		 --eval "(generate-run-tests-batch-and-exit)";


test-ert: ## Run ert tests
test-ert: 
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l generate-ert-tests.el \
		 --eval "(generate-run-tests-batch-and-exit)";

test-runner: ## Run test-runner tests
test-runner: 
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l generate-test-runner-tests.el \
		 --eval "(ert-run-tests-batch-and-exit)";

test-org: ## Run org-mode tests
test-org: 
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l generate-org-mode-tests.el \
		 --eval "(ert-run-tests-batch-and-exit)";

scratch: 
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l scratch.el

test-scratch: 
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l scratch.el \
		 --eval "(ert-run-tests-batch-and-exit)";

check:
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l generate-check.el \
		 --eval "(ert-run-tests-batch-and-exit)";




