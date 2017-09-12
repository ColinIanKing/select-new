@bad1 depends on after@
expression e;
@@

 e->FLD

@bad2 depends on after@
expression e;
@@

 e.FLD

@bad3 depends on after@
identifier i;
type STR;
expression e;
@@

STR i = {
  .FLD
    = e,
};

@depends on !bad1 && !bad2 && !bad3@
expression e;
@@

- e->FLD

@depends on !bad1 && !bad2 && !bad3@
expression e;
@@

- e.FLD

@depends on !bad1 && !bad2 && !bad3@
identifier i;
type STR;
expression e;
@@

STR i = {
-  .FLD
    = e,
};
