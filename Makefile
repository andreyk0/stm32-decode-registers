PROJECT:=stm32-decode-registers

build: STMicro.tgz
	stack build $(PROJECT)

STMicro.tgz: cmsis-svd/data/STMicro
	cd cmsis-svd/data && tar czvf ../../STMicro.tgz STMicro

cmsis-svd/data/STMicro:
	git submodule init
	git submodule update
