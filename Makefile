srcdir = ./src
builddir = ./build

OBJECTS = $(builddir)/epoll.o $(builddir)/server.o

all: rooster

# csc outputs C object files to same directory as .c file, so gcc C files ourselves without csc
$(builddir)/%.o: $(srcdir)/%.c
	gcc $< -o $@ -c -fno-strict-aliasing -DHAVE_CHICKEN_CONFIG_H -DC_ENABLE_PTABLES -Os -fomit-frame-pointer -fPIC -DPIC -DC_SHARED -I/usr/local/include

$(builddir)/%.o: $(srcdir)/%.scm
	csc -scv -o $@ $<

rooster: $(OBJECTS)
	csc -sv -o rooster.so $(OBJECTS)

clean:
	@echo Cleaning up.
	rm -f $(builddir)/*.o; rm rooster.so
	@echo Done.
