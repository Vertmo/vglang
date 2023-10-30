include ../../Makefile.config
include ../../Makefile.include

game.exe: main.c game.c
	gcc -o $@ $<

game.c: move.vg world.vg
	$(VGLANGC) $^ > game.c

clean:
	rm -f game.*
