SRC=src/

all: bnfc insc_jvm insc_llvm

bnfc: $(SRC)/Instant.cf
		cd $(SRC) && bnfc -m -haskell Instant.cf && make && cd ..

insc_jvm:

insc_llvm:
