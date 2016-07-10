include ../Make.options


MCFLAGS_FOR_UUID = $(FLAGS_FOR_RPC_LIB) --install-prefix "$(INSTALL_PREFIX)"

.PHONY: default
default:
	$(MMC) $(MCFLAGS_FOR_UUID) --make libmercury_uuid

.PHONY: rebuild
rebuild:
	$(MMC) $(MCFLAGS_FOR_UUID) --rebuild libmercury_uuid

.PHONY: install
install:
	$(MMC) $(MCFLAGS_FOR_UUID) --make libmercury_uuid.install

tags: $(wildcard *.m)
	mtags $^

.PHONY: clean
clean:
	$(MMC) --make mercury_uuid.clean

.PHONY: realclean
realclean:
	$(MMC) --make mercury_uuid.realclean
	/bin/rm -rf Mercury
	/bin/rm -f $(wildcard *.err) $(wildcard *.mh)
	/bin/rm -f tags
