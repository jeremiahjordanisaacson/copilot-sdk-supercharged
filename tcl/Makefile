# Makefile for the Copilot SDK for Tcl
#
# Targets:
#   install   - Copy library to Tcl package path
#   test      - Run tcltest suite
#   clean     - Remove build artifacts
#   lint      - Basic syntax check
#   help      - Show this help

TCL       ?= tclsh
LIB_DIR    = lib/copilot
TEST_DIR   = tests
INSTALL_DIR ?= $(shell echo 'puts [lindex $$auto_path 0]' | $(TCL))

.PHONY: install test clean lint help

help:
	@echo "Copilot SDK for Tcl"
	@echo ""
	@echo "Targets:"
	@echo "  install   - Install library to Tcl auto_path"
	@echo "  test      - Run tests with tcltest"
	@echo "  clean     - Remove build artifacts"
	@echo "  lint      - Syntax-check all .tcl files"
	@echo "  help      - Show this message"

install:
	@echo "Installing to $(INSTALL_DIR)/copilot/"
	@mkdir -p "$(INSTALL_DIR)/copilot"
	@cp $(LIB_DIR)/*.tcl "$(INSTALL_DIR)/copilot/"
	@echo "Done. Add to auto_path or use: package require copilot::client"

test:
	@echo "Running tests..."
	$(TCL) $(TEST_DIR)/client_test.tcl

clean:
	@echo "Cleaning up..."
	@rm -f *.log
	@echo "Done."

lint:
	@echo "Syntax-checking .tcl files..."
	@for f in $(LIB_DIR)/*.tcl; do \
		echo "  Checking $$f"; \
		$(TCL) -c "source $$f" 2>&1 || true; \
	done
	@echo "Done."
