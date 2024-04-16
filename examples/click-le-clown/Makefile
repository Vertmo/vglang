include ../../Makefile.config
include ../../Makefile.include

# COMPONENTS = move.vg
ENTITIES = clown.vg
SYSTEMS =
SCENES = world.vg

vglang: $(COMPONENTS) $(ENTITIES) $(SYSTEMS) $(SCENES)
	$(VGLANGC) $^

clean:
	rm -f game.*

.PHONY: vglang clean
