@r1 depends on before@
identifier i,f1;
@@

STR i = {
  .FLD = \(f1\|&f1\),
};

@r2 depends on before expression@
STR e;
STR *ep;
identifier f2;
@@

  <+...\(e.FLD\|ep->FLD\)...+> = \(f2\|&f2\)

@r depends on before@
identifier f,r1.f1,r2.f2;
@@

(
(
 f1
|f2
)
&
 f
)

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
