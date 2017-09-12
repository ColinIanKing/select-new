@bad depends on before || after@
flexible expression list[n] es;
@@

FN(es)

@depends on !bad && (before || after)@
expression e;
@@

FN(...,
+ e,
  ...)
