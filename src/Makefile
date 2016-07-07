include ../Make.options

.PHONY: default
default:
	$(MMC) --make libmercury_uuid

.PHONY: rebuild
rebuild:
	$(MMC) --rebuild libmercury_uuid

.PHONY: install
install:
	$(MMC) --make libmercury_uuid.install

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
