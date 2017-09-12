@bad depends on before@
@@

EXP(...);

@depends on !bad@
symbol EXP;
@@

- EXP

@depends on !bad@
expression e;
@@

- e
  = <+... EXP ...+>