# OCaml ray tracer, 9/23/2017.

# Apparently order of sources matters. It probably generates the necessary files 
# and keeps them on-hand throughout the compilation process.
TARGET=main
SOURCES=constants.ml vec3.ml ray.ml camera.ml material.ml intersection.ml sphere.ml world.ml main.ml

all:
	ocamlopt -unsafe -nodynlink -inline 20 -o $(TARGET) $(SOURCES)
	rm -f *.cmi *.cmo *.cmx *.o
