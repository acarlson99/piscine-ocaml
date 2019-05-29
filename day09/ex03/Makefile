CC = ocamlbuild
NAME = main.native

all: $(NAME)

$(NAME):
	$(CC) $(NAME)

clean:
	rm -rf _build

fclean: clean
	rm -f $(NAME)

re: fclean all
