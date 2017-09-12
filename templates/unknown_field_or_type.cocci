@bad1 depends on after@
STR *e;
@@

 e->FLD

@bad2 depends on after@
STR e;
@@

 e.FLD

@bad3 depends on after@
identifier i;
expression e;
@@

STR i = {
  .FLD
    = e,
};

@depends on !bad1 && !bad2 && !bad3@
STR *e;
@@

- e->FLD

@depends on !bad1 && !bad2 && !bad3@
STR e;
@@

- e.FLD

@depends on !bad1 && !bad2 && !bad3@
identifier i;
expression e;
@@

STR i = {
-  .FLD
    = e,
};




@bad depends on after@
@@

  STR

@depends on !bad@
@@

- STR



@badc depends on after@
expression e;
symbol FLD;
@@

  container_of(e,STR,FLD)

@depends on !badc@
expression e;
@@

container_of(e,
- STR,
  FLD)

@depends on !badc@
expression e;
@@

container_of(e,STR,
- FLD
 )
