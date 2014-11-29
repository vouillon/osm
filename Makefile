OCAMLC=ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep

OPTCOMPFLAGS =-package zip,bigarray,unix,cairo2.lablgtk2 -inline 3 -g \
  $(DIRS)
COMPFLAGS = $(DIRS)
DEPFLAGS= $(DIRS)
OPTLINKFLAGS=$(OPTCOMPFLAGS) -linkpkg #-unsafe

#####

GENERIC=\
  util.cmx debug.cmx bitvect.cmx \
  bytearray_stubs.o bytearray.cmx mapped_file.cmx task_stubs.o task.cmx \
  binary_heap.cmx pqueue.cmx data_stream.cmx protobuf.cmx \
  lru_cache.cmx

DATABASE=\
  column_opt.o column.cmx sort.o sorting.cmx join.cmx \
  projection.cmx table.cmx dictionary.cmx column_ops.cmx rtree.cmx

OSM=\
  lib/osm_geometry.cmx

ROUTING=\
  routing_profile.cmx profile_car.cmx profile_pedestrian.cmx

DIRS=-I generic -I database -I osm -I osm/lib

OBJS= $(addprefix generic/,$(GENERIC)) $(addprefix database/,$(DATABASE)) \
      $(addprefix osm/,$(OSM))

BINARIES=query load multipolygons linear surfaces highway contraction display coastline

all: $(BINARIES)

query: $(OBJS) database/query.cmx
	$(OCAMLOPT) $(OPTLINKFLAGS) -o $@ $^

load: $(OBJS) osm/parser.cmx osm/prepare.cmx
	$(OCAMLOPT) $(OPTLINKFLAGS) -o $@ $^

multipolygons: $(OBJS) osm/multipolygons.cmx
	$(OCAMLOPT) $(OPTLINKFLAGS) -o $@ $^

linear: $(OBJS) osm/lib/osm_category.cmx osm/linear.cmx
	$(OCAMLOPT) $(OPTLINKFLAGS) -o $@ $^

surfaces: $(OBJS) osm/lib/osm_douglas_peucker.cmx osm/lib/osm_category.cmx osm/surfaces.cmx
	$(OCAMLOPT) $(OPTLINKFLAGS) -o $@ $^

highway: $(OBJS) $(addprefix osm/, $(ROUTING)) osm/highway.cmx
	$(OCAMLOPT) $(OPTLINKFLAGS) -o $@ $^

contraction: $(OBJS) osm/lib/osm_contraction.cmx osm/contraction.cmx
	$(OCAMLOPT) $(OPTLINKFLAGS) -o $@ $^

display: $(OBJS) osm/routing.cmx osm/line_smoothing.cmx osm/lib/osm_douglas_peucker.cmx osm/lib/osm_category.cmx osm/lib/osm_display.cmx osm/display.cmx
	$(OCAMLOPT) $(OPTLINKFLAGS) -o $@ $^

coastline: $(OBJS) osm/lib/osm_category.cmx osm/lib/osm_douglas_peucker.cmx osm/lib/osm_clipping.cmx osm/lib/osm_coastline.cmx osm/coastline.cmx
	$(OCAMLOPT) $(OPTLINKFLAGS) -o $@ $^

clean::
	rm -f $(BINARIES)

#####

clean::
	find . -regex ".*\\.\(cm[oix]\|o\)" | xargs rm -f
realclean:: clean

.SUFFIXES: .cmo .cmi .cmx .ml .mli .o .c

.ml.cmo:
	$(OCAMLC) $(BYTECOMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OPTCOMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(COMPFLAGS) -c $<

.c.o:
	$(OCAMLC) -ccopt -O3 -c -ccopt "-o $@" $<

depend:
	find . -regex ".*\\.mli?" | LC_ALL=C sort | xargs \
	$(OCAMLDEP) $(DEPFLAGS) $$i \
	> .depend

include .depend
