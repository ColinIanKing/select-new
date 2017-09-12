@bad1 depends on before@
STR e;
@@

e.FLD(...);

@bad2 depends on before@
STR *e;
@@

e->FLD(...);

@depends on !bad1@
STR e;
@@

- e.FLD

@depends on !bad2@
STR *e;
@@

- e->FLD

@depends on !bad1@
expression e0;
STR e;
@@

- e0
  = <+... e.FLD ...+>

@depends on !bad2@
expression e0;
STR *e;
@@

- e0
  = <+... e->FLD ...+>
