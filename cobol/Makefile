# Makefile for the GitHub Copilot SDK for COBOL (GnuCOBOL)
#
# Requirements: GnuCOBOL (cobc) 3.0 or later
# Install: apt install gnucobol  OR  brew install gnucobol

COBC       = cobc
COBFLAGS   = -x -free -std=cobol2014
COPYBOOKDIR = src
SRCDIR     = src
EXDIR      = examples
TESTDIR    = tests
BUILDDIR   = build

SOURCES    = $(SRCDIR)/COPILOT-CLIENT.cob \
             $(SRCDIR)/COPILOT-JSONRPC.cob \
             $(SRCDIR)/COPILOT-SESSION.cob

COPYBOOKS  = $(SRCDIR)/COPILOT-TYPES.cpy \
             $(SRCDIR)/COPILOT-VERSION.cpy

EXAMPLE    = $(EXDIR)/BASIC-EXAMPLE.cob
TEST       = $(TESTDIR)/TEST-CLIENT.cob

.PHONY: all clean lib example test help

all: lib example test

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

# Build all source modules as callable objects
lib: $(BUILDDIR)
	$(COBC) -m -free -std=cobol2014 -I $(COPYBOOKDIR) \
		$(SRCDIR)/COPILOT-JSONRPC.cob -o $(BUILDDIR)/COPILOT-JSONRPC.so
	$(COBC) -m -free -std=cobol2014 -I $(COPYBOOKDIR) \
		$(SRCDIR)/COPILOT-SESSION.cob -o $(BUILDDIR)/COPILOT-SESSION.so
	$(COBC) -m -free -std=cobol2014 -I $(COPYBOOKDIR) \
		$(SRCDIR)/COPILOT-CLIENT.cob -o $(BUILDDIR)/COPILOT-CLIENT.so
	@echo "=== COBOL SDK modules built ==="

# Build the example program
example: lib $(BUILDDIR)
	$(COBC) $(COBFLAGS) -I $(COPYBOOKDIR) \
		$(EXAMPLE) -o $(BUILDDIR)/basic-example \
		-L $(BUILDDIR)
	@echo "=== Example built: $(BUILDDIR)/basic-example ==="

# Build and run the test program
test: lib $(BUILDDIR)
	$(COBC) $(COBFLAGS) -I $(COPYBOOKDIR) \
		$(TEST) -o $(BUILDDIR)/test-client \
		-L $(BUILDDIR)
	@echo "=== Test built: $(BUILDDIR)/test-client ==="
	@echo "Running tests..."
	COB_LIBRARY_PATH=$(BUILDDIR) ./$(BUILDDIR)/test-client
	@echo "=== Tests complete ==="

# Syntax check only (no linking)
check:
	$(COBC) -fsyntax-only -free -std=cobol2014 -I $(COPYBOOKDIR) \
		$(SOURCES) $(EXAMPLE) $(TEST)
	@echo "=== Syntax check passed ==="

clean:
	rm -rf $(BUILDDIR)
	@echo "=== Clean complete ==="

help:
	@echo "Copilot SDK for COBOL - Build targets:"
	@echo "  make all      - Build library, example, and tests"
	@echo "  make lib      - Build SDK shared modules"
	@echo "  make example  - Build example program"
	@echo "  make test     - Build and run tests"
	@echo "  make check    - Syntax check only"
	@echo "  make clean    - Remove build artifacts"
