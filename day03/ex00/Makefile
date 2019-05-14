NAME = ft_graphics.native
LIB = -lib graphics

all: ${NAME}

${NAME}:
	ocamlbuild ${LIB} ${NAME}

clean:
	rm -rf _build ${NAME}
	rm -f ${NAME}

re: clean all
