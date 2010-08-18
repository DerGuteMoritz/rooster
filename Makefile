srcdir = ./src
builddir = ./build

OBJECTS = $(builddir)/rooster.o

all: rooster

$(builddir)/%.o: $(srcdir)/%.scm
	csc -scv -o $@ $<

rooster: $(OBJECTS)
	csc -sv -o rooster.so $(OBJECTS)

clean:
	@echo Cleaning up.
	rm -f $(builddir)/*.o; rm rooster.so
	@echo Done.
