PROJECT:=stm32-decode-registers

build: STMicro.tgz
	stack build $(PROJECT)
	ln -sf $$(stack path --local-install-root)/bin .

STMicro.tgz: cmsis-svd/data/STMicro
	cd cmsis-svd/data && tar czvf ../../STMicro.tgz STMicro

cmsis-svd/data/STMicro:
	git submodule init
	git submodule update
