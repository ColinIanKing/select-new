// discard calls where the before and after arg lists are identical.
// can't require that the arg list size doesn't change, because it might go
// up and down, cf phy-exynos-mipi-video.c_069d2e2/step1.cocci

@bad depends on before || after@
expression list es;
@@

FN(es)

@depends on !bad && (before || after)@
expression e1,e2;
flexible expression list[ARG] es;
@@

FN(es,
- e1
+ e2
  ,...)

// -------------------------------------------------------------

@r@
identifier f;
flexible expression list[ARG] es;
@@

FN(es,\(f\|&f\),...)

@@
type T;
identifier r.f;
@@

- T
  f(...) { ... }

@@
identifier r.f;
parameter p;
@@

f(...,
- p
  ,...) { ... }

@@
identifier r.f;
parameter p;
@@

f(...,
+ p
  ,...) { ... }
