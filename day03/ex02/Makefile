NAME = test
BOI = uncipher.native

all: ${NAME}

${NAME}:
	ocamlbuild ${BOI} && mv ${BOI} ${NAME}

clean:
	rm -rf _build ${NAME}
	rm -f ${NAME}

re: clean all
