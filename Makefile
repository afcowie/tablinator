all: experiment

experiment:
	stack build tablinate

tags:
	codex update

pdf:
	pandoc -o junk.pdf examples/junk.markdown
