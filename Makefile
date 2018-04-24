all:
	agda ParseTree.agda -c --no-main  --verbose=0
	agda Refactoring.agda -c --no-main  --verbose=0
