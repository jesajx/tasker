
.PHONY : all
all: tasker

tasker : tasker.hs
	ghc --make $^


tests : tasker
	for f in tests/*.tsk ; do ./tasker "$$f" 2>&1 > "$$f".txt ; done

.PHONY : clean
clean :
	rm -f tasker{,.hi,.o} Tasker{.hi,.o} tests/test*.tsk.txt
