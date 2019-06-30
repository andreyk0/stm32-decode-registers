PROJECT:=stm32-decode-registers
SVDTGZ:=STMicro.tgz

.DEFAULT: all

all: test build

test: $(SVDTGZ)
	stack test --fast $(PROJECT)

build: $(SVDTGZ)
	stack build --fast $(PROJECT)
	rm -f bin && ln -sf $$(stack path --dist-dir)/build/$(PROJECT)-exe ./bin

build-profile: $(SVDTGZ)
	stack build --no-strip --profile $(PROJECT)
	rm -f bin && ln -sf $$(stack path --dist-dir)/build/$(PROJECT)-exe ./bin

build-dist: $(SVDTGZ)
	stack build $(PROJECT)
	rm -f bin && ln -sf $$(stack path --local-install-root)/bin ./
	upx bin/*
#stack build --verbose --flag $(PROJECT):static $(PROJECT)

$(SVDTGZ): cmsis-svd/data/STMicro
	cd cmsis-svd/data && tar czvf ../../$(SVDTGZ) STMicro

cmsis-svd/data/STMicro:
	git submodule init
	git submodule update

# Note hoogle hates TH-embedded SVD data, truncate it first
hoogle:
	touch $(SVDTGZ) && truncate $(SVDTGZ)
	stack hoogle --rebuild
	rm -f $(SVDTGZ)
	stack hoogle --server

clean:
	stack clean

clobber:
	git clean -fdx .


.PHONY: \
	all \
	build \
	build-dist \
	build-profile \
	clean \
	clobber \
	hoogle \
	test \
