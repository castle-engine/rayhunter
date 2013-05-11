.PHONY: compile
compile:
	./compile.sh

# Run also "dircleaner . clean" here to really clean
.PHONY: clean
clean:
	rm -f rayhunter rayhunter.exe
	rm -Rf rayhunter.app
