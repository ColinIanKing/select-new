@@
identifier i,f1;
@@

STR i = {
  .FLD =
- f1
  ,
};

@expression@
STR e;
STR *ep;
identifier f2;
@@

  <+...\(e.FLD\|ep->FLD\)...+> =
- f2

@r1@
identifier i,f1;
@@

STR i = {
  .FLD = f1,
};

@r2 expression@
STR e;
STR *ep;
identifier f2;
@@

  <+...\(e.FLD\|ep->FLD\)...+> = f2

@r@
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
  f;
