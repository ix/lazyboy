all:
	rgbasm -o $(NAME).obj $(NAME).asm
	rgblink -m $(NAME).map -n $(NAME).sym -o $(NAME).gb $(NAME).obj
	rgbfix -p0 -v -t$(NAME) -n1 -flhg $(NAME).gb 
