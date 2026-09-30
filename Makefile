EMACS ?= emacs
PKG = generate
LOAD_PATH  += -L .
LOAD_PATH  += -L ./tests


.PHONY: test-primitives test-runner test-org check

clean: ## Clean up all temporary files created during testing/runtime.
	find . -name "*.elc" -type f -delete

test-primitives: ## Run primitives tests
test-primitives: clean
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l generate-primitives-tests.el \
		 --eval "(generate-run-tests-batch-and-exit)";


test-ert: ## Run ert tests
test-ert: clean
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l generate-ert-tests.el \
		 --eval "(generate-run-tests-batch-and-exit)";

test-runner: ## Run test-runner tests
test-runner: clean
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l generate-test-runner-tests.el \
		 --eval "(ert-run-tests-batch-and-exit)";

test-org: ## Run org-mode tests
test-org: clean
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l generate-org-mode-tests.el \
		 --eval "(generate-run-tests-batch-and-exit)";

scratch: clean
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l scratch.el

test-scratch: clean
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l scratch.el \
		 --eval "(ert-run-tests-batch-and-exit)";

check:
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l generate-check.el \
		 --eval "(ert-run-tests-batch-and-exit)";
