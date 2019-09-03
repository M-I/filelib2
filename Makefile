PROJECT = filelib2
PROJECT_DESCRIPTION = Helper functions for files, symbolic links and folders
PROJECT_VERSION = 1.0.0

EDOC_OPTS = {dir, "docs"}
docs:: edoc

include erlang.mk
