all:
	rgbasm -o hello.obj hello.asm
	rgblink -m hello.map -n hello.sym -o hello.gb hello.obj
	rgbfix -p0 -v -tHELLO -n1 -flhg hello.gb 
