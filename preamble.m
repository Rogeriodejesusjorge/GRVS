(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



d = 4;
xx = {t,r,\[Theta],\[Phi]};
xxVor={t[\[Tau]],r[\[Tau]],\[Theta][\[Tau]],\[Phi][\[Tau]]};
xxt={t,r[t],\[Theta][t],\[Phi][t]};
gg = { {  1-(2 m)/r,   0         ,      0       ,     0                         },
          {     0         , - (1/(1-(2 m)/r)),      0       ,     0                         },
          {     0         ,     0       ,      -r^2     ,     0                        },
          {     0         ,     0       ,      0       , -r^2 Sin[\[Theta]]^2  }};
nsteps=10000;
lagrangian=1;
m=1;rs=2m;\[Tau]min=0;\[Tau]max=63.42;\[Gamma]=-0.07;L=0;rmin=1.01rs;rmax=5 rs;
\[Chi]=1;q=1;temperature=1;
AA = {A0[r],  0,  0,  0};
\[Rho][r_]=\[Alpha] P[r];
uu={g[1,1],0,0,0};
TTf=Table[(\[Rho][r]+P[r]) uu[[i]] uu[[j]]-gg[[i,j]] P[r],{i,1,d},{j,1,d}];
