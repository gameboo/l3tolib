L3 to lib
=========

This program exports an L3 ISA to a set of SML FFI declarations so that it can
be compiled into a library / shared object. The intention is to be able to
easily write system simulators where the ISA can be described in L3, and
plugged into other components implemented with other languages (cache hierarchy
modelled in C/C++ for example...).

Currently, the program simply parses the *.sig* file produced by the L3
compiler to find definitions that can be exported. User defined L3 types are
not supported. The SML BitsN library (implementing the L3 "bits" types) can be
turned into an FFI type for all bits types up to 64 bits (included).

Two positional arguments are expected when calling the tool: the path to the
*.sig* file followed by a string that will be used as a prefix for exported
symbols and for the name of the produced FFI SML file.

Example
=======

Running `./l3tolib my_isa_model.sig my_isa` will produce `my_isa_ffi.sml` that
can then be used as part of a *.mlb* mlton file to build a library using the
`-format library` or `-format archive` option.
```
mlton -output libmy_isa.so -default-ann 'allowFFI true' -format library -export-header my_isa.h -default-type intinf lib_my_isa.mlb
```
The generated *.so* and *.h* can then be used to build, for example, some C
code using the exported L3 functions and implementing the imported functions.
See [the mlton FFI documentation](http://mlton.org/ForeignFunctionInterface)
for more information on the exported types.
```
gcc my_c_project.c -I $(PATH_TO_HEADER) -L $(PATH_LO_LIB) -lmy_isa
```

TODO
====

* ~~Bug in the support of BitsN~~
* Automate the running of mlton
