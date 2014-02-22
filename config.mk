CC              :=cc
CABAL           :=cabal
CFLAGS          :=-O2 -Wall -I/opt/boxen/homebrew/Cellar/llvm/3.3/include -I/opt/boxen/homebrew/Cellar/libffi/3.0.13/lib/libffi-3.0.13/include $(CFLAGS)
LDFLAGS := -L/opt/boxen/homebrew/Cellar/llvm/3.3/lib -L/opt/boxen/homebrew/Cellar/libffi/3.0.13/lib $(LDFLAGS)
CABALFLAGS	:= -f FFI -f LLVM
## Disable building of Effects
#CABALFLAGS :=-f NoEffects

ifneq (, $(findstring bsd, $(MACHINE)))
	GMP_INCLUDE_DIR      :=
else
	GMP_INCLUDE_DIR      :=-I/usr/local/include
endif

MACHINE         := $(shell $(CC) -dumpmachine)
ifneq (, $(findstring darwin, $(MACHINE)))
	OS      :=darwin
else
ifneq (, $(findstring cygwin, $(MACHINE)))
	OS      :=windows
else
ifneq (, $(findstring mingw, $(MACHINE)))
	OS      :=windows
else
	OS      :=unix
endif
endif
endif

ifeq ($(OS),darwin)
	SHLIB_SUFFIX    :=.dylib
else
ifeq ($(OS),windows)
	SHLIB_SUFFIX    :=.DLL
else
	SHLIB_SUFFIX    :=.so
endif
endif

