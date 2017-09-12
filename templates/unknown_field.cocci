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

@bad4 depends on after@
identifier i;
expression e;
@@

STR i[...] = {...,{
  .FLD
    = e,
},...,};

@depends on !bad1 && !bad2 && !bad3 && !bad4@
STR *e;
@@

- e->FLD

@depends on !bad1 && !bad2 && !bad3 && !bad4@
STR e;
@@

- e.FLD

@depends on !bad1 && !bad2 && !bad3 && !bad4@
identifier i;
expression e;
@@

STR i = {
-  .FLD
    = e,
};

@depends on !bad1 && !bad2 && !bad3 && !bad4@
identifier i;
expression e;
@@

STR i[...] = {...,{
-  .FLD
    = e,
},...,};
