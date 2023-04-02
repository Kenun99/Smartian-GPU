BUILDDIR=$(shell pwd)/build

all: $(BUILDDIR) Runner Smartian

clean:
	@dotnet clean -c Release
	@rm -rf $(BUILDDIR)

$(BUILDDIR):
	@mkdir -p $(BUILDDIR)

Runner:
	@cp /home/weimin/build/runner/librunner.so $(BUILDDIR)
	
Smartian:
	@dotnet build -c Release -o $(BUILDDIR)

.PHONY: all clean Smartian
