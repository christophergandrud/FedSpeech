################
# Makefile for Stan Speeches
# Christopher Gandrud
# Updated 3 March 2015
# Influenced by Rob Hyndman (31 October 2012)
# See: http://robjhyndman.com/researchtips/makefiles/
################

# Key variables to define
RDIR = .
RDATA = .RData

# Create list of R source files
RSOURCE = $(wildcard $(RDIR)/*.R)

# Files to indicate when the RSOURCE file was run
OUT_FILES = $(RSOURCE:.R=.Rout)

# Default target
all: $(OUT_FILES)

# Run the RSOURCE files
$(RDIR)/%.Rout: $(RDIR)/%.R
	R CMD BATCH $<

# Remove R Out Files
clean:
	rm -fv $(OUT_FILES)

# Remove .RData file
cleanData:
	rm -fv $(RDATA)
