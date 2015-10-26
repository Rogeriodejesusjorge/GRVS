(* ::Package:: *)

(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]];*)
(*<<preamble.m*)


(* ::Input:: *)
(*sp := Simplify;fsp:=FullSimplify;sol :=Solve;cl:=Collect;cf :=Coefficient;ex:=Expand; sr:=Series;*)


(* ::Input:: *)
(*(* Metric ansatz *)*)
(*(* ds^2 = -F(r) dt^2 +G(r) dr^2 +r^2 d\[Theta]^2 +r^2 (sin \[Theta])^2 d\[Phi]^2 *)*)


(* ::Input:: *)
(*(* Metric: g_{ij}, Inverse metric: g^{ij} *)*)
(*Do[x[i] = xx[[i]],{i,1,d}]*)
(*Do[g[i,j]=gg[[i,j]],{i,1,d},{j,1,d}];*)
(*ivg=Inverse[gg];*)
(*Do[ig[i,j]=ivg[[i,j]],{i,1,d},{j,1,d}];*)


(* ::Input:: *)
(*(* Vector potential: A_{i}  *)*)
(*Do[A[i] = AA[[i]], {i,1,d}]*)


(* ::Input:: *)
(*(* Fluid Tensor part *)*)
(*Do[u[i] = uu[[i]], {i,1,d}]*)
(*Do[Tf[i,j] = TTf[[i,j]], {i,1,d},{j,1,d}]*)


(* ::Input:: *)
(*(* Stress-energy-momentum tensor for matter fields *)*)
(*(* For the electromagnetic fields, T_{ab} = 1/(4 \[Pi]) (F_{ac} F_{b}^{c} -1/4 g_{ab} F_{cd} F^{cd})  *)*)
(*(* Field strength: F_{ab} = \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(a\)]\ A_\){b} -\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(b\)]\ A_\){a}  , where A_{a} is the vector potential    *) *)


(* ::Input:: *)
(*(* Field strength: F_{ij} ,  F_{i}^{j} = g^{jk} F_{ik}  *)*)
(*Do[F[i,j] = sp[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x[i]\)]\ \(A[j]\)\) -\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x[j]\)]\ \(A[i]\)\)],{i,1,d},{j,1,d}]*)


(* ::Input:: *)
(*(* EM Tensor part *)*)
(*Do[Tem[i,j] = sp[1/(4 \[Pi]) (\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(m = 1\), \(d\)]\(F[i, m]*\(( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(n = 1\), \(d\)]ig[m, n]*F[j, n])\)\)\) -1/4 g[i,j]*(\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(k = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(l = 1\), \(d\)]F[k, l]*\((\ *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(m = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(n = 1\), \(d\)]ig[k, m]*ig[l, n]*F[m, n]\))\)\)\)))],{i,1,d},{j,1,d}]*)


(* ::Input:: *)
(*(* Stress-energy tensor: T_{ij}  *)*)
(*Do[T[i,j] = sp[Tem[i,j]+Tf[i,j]],{i,1,d},{j,1,d}]*)


(* ::Input:: *)
(*(* Matrix form of T_{ij}  *)*)
(*TT=Table[T[i,j],{i,4},{j,4}];*)
(*MatrixForm[TT];*)


(* ::Input:: *)
(*(* Cristoffel symbol: \[CapitalGamma]^{k}_{ij}  *)*)
(*(* \[CapitalGamma]_{ab}^{c} = 1/2*g^{cd}*(\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(a\)]\ g_\){bd} +\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(b\)]\ g_\){ad} -\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(d\)]\ g_\){ab})  *)*)
(*Do[\[CapitalGamma][k,i,j] = sp[1/2 \!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(m = 1\), \(d\)]\(ig[k, m] \(( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x[i]\)]\ g[j, m] + *)
(*\*SubscriptBox[\(\[PartialD]\), \(x[j]\)]\ g[i, m] - *)
(*\*SubscriptBox[\(\[PartialD]\), \(x[m]\)]\ g[i, j])\)\)\)],{i,1,d},{j,1,d},{k,1,d}]*)


(* ::Input:: *)
(*(* Riemann curvature tensor:  R_{ijkl}  *)*)
(*Do[R[i,j,k,l] = sp[ \!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(n = 1\), \(d\)]\(g[l, n] \(( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x[j]\)]\ \[CapitalGamma][n, i, k] - *)
(*\*SubscriptBox[\(\[PartialD]\), \(x[i]\)]\ \[CapitalGamma][n, j, k]\  + *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(m = 1\), \(d\)]\((\[CapitalGamma][m, i, k] \[CapitalGamma][n, m, j]\  - \ \[CapitalGamma][m, j, k] \[CapitalGamma][n, m, i])\))\)\)\)],{i,1,d},{j,1,d},{k,1,d},{l,1,d}]*)


(* ::Input:: *)
(*(* Kretschmann invariant (i.e., Riemann tensor squared): KI = R_{abcd} R^{abcd} *)*)
(*KI=sp[\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(k = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(l = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(m = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(n = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(p = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(q = 1\), \(d\)]ig[i, m]\ ig[j, n]\ ig[k, p]\ ig[l, q]\ R[i, j, k, l]\ R[m, n, p, q]\)\)\)\)\)\)\)\)];*)


(* ::Input:: *)
(*(* Ricci tensor:  R_{ij}  *)*)
(*Do[R[i,j] = sp[ \!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(k = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(m = 1\), \(d\)]ig[k, m] R[i, k, j, m]\)\)],{i,1,d},{j,1,d}]*)


(* ::Input:: *)
(*(* Ricci scalar: R  *)*)
(*Do[RR = sp[ \!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(d\)]ig[i, j] R[i, j]\)\)],{i,1,d},{j,1,d}]*)


(* ::Input:: *)
(*(* Einstein tensor: G_{ij} *)*)
(*Do[G[i,j] = sp[ R[i,j] -1/2 g[i,j] RR ], {i,1,d},{j,1,d}]*)


(* ::Input:: *)
(*(* Einstein equations: EQ_{ij} *)*)
(*(* EQ_{ab} = G_{ab} -8 \[Pi] T_{ab} = 0 *)*)
(*\[CapitalLambda]=\[CapitalLambda];*)
(*Do[EQ[i,j] = sp[G[i,j] +\[CapitalLambda] g[i,j]-8*\[Pi]*T[i,j]],{i,1,d},{j,1,d}]*)


(* ::Input:: *)
(*(* Covariant derivative of A_{a}: DA_{ij}  *)*)
(*(* \[Del]_{a} A_{b}  *)*)
(*Do[DA[i,j] = sp[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x[i]\)]\ \(A[j]\)\) -\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(k = 1\), \(d\)]\(\[CapitalGamma][k, i, j]*A[k]\)\)],{i,1,d},{j,1,d}]*)


(* ::Input:: *)
(*(* Second derivative of A_{a}: DDA_{ijk}  *)*)
(*(* \[Del]_{a} \[Del]_{b} A_{c}  *)*)
(*Do[DDA[i,j,k] = sp[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x[i]\)]\ \(DA[j, k]\)\) -\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(m = 1\), \(d\)]\(\[CapitalGamma][m, i, j]*DA[m, k]\)\) -\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(m = 1\), \(d\)]\(\[CapitalGamma][m, i, k]*DA[j, m]\)\)],{i,1,d},{j,1,d},{k,1,d}]*)


(* ::Input:: *)
(*(* Lorentz gauge condition: LG  *)*)
(*(* \[Del]^{a} A_{a} = 0  *)*)
(*LG = sp[\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(d\)]ig[i, j]*DA[j, i]\)\)] *)


(* ::Input:: *)
(*(* Matter field equations: MEQ_{i}  *)*)
(*(* \[Del]^{a} F_{ab} = \[Del]^{a} \[Del]_{a} A_{b} -R_{b}^{a} A_{a} = 0  *)*)
(*Do[MEQ[i] = sp[\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(k = 1\), \(d\)]ig[j, k]*DDA[k, j, i]\)\) -\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(k = 1\), \(d\)]ig[j, k]*R[i, k]*A[j]\)\)],{i,1,d}]*)


(* ::Input:: *)
(*sp[ex[EQ[1,1]]]*)
(*sp[MEQ[1]]*)


(* ::Input:: *)
(*(* Display usuful quantities *)*)


(* ::Input:: *)
(*(*Do[Print["\[CapitalGamma]^{",x[k],"}_{",x[i],",",x[j],"} = ",\[CapitalGamma][k,i,j]],{k,1,d},{i,1,d},{j,1,d}]*)*)


(* ::Input:: *)
(*(*Do[Print["R_{",x[i],",",x[j],",",x[k],",",x[l],"} = ",R[i,j,k,l]],{i,1,d},{j,1,d},{k,1,d},{l,1,d}];*)*)


(* ::Input:: *)
(*(*Print["R_{abcd} R^{abcd} = ",KI//ex]*)*)


(* ::Input:: *)
(*(*Do[Print["R_{",x[i],",",x[j],"} = ",R[i,j]],{i,1,d},{j,1,d}]*)*)
(**)


(* ::Input:: *)
(*(*Print["R = ",RR]*)*)


(* ::Input:: *)
(*(*Do[Print["G_{",x[i],",",x[j],"} = ",G[i,j]],{i,1,d},{j,1,d}]*)*)


(* ::Input:: *)
(*(*Do[Print["T_{",x[i],",",x[j],"} = ",T[i,j]],{i,1,d},{j,1,d}]*)*)


(* ::Input:: *)
(*(*Do[Print["EQ_{",x[i],",",x[j],"} = ",EQ[i,j]],{i,1,d},{j,1,d}]*)*)


(* ::Input:: *)
(*(* Nonvanishing Einstein equations  *)*)
(*EQ[1,1]//ex;*)
(*EQ[2,2]//ex;*)
(*EQ[3,3]//ex;*)


(* ::Input:: *)
(*(*Do[Print["MEQ[",x[i],"]= ",MEQ[i]],{i,1,d}]*)*)


(* ::Input:: *)
(*(* Nonvanishing field equations for matter  *)*)
(*(*MEQ[1]//fsp*)*)


(* ::Input:: *)
(*cl[MEQ[1]//ex,Derivative[1][A0][r]];*)


(* ::Input:: *)
(*(* Lorentz gauge condition *)*)
(*LG*)


(* ::Input:: *)
(*(* Covariant Divergence of the Energy Momentum Tensor *)*)
(*Do[ConT[i,j]=\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(a = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(b = 1\), \(d\)]ig[i, a] ig[j, b] T[a, b]\)\),{i,1,d},{j,1,d}]*)
(*Do[DT[i] = sp[\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(d\)]\(( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x[j]\)]\ ConT[i, j])\)\) +\!\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(d\)]\( *)
(*\*UnderoverscriptBox[\(\[Sum]\), \(k = 1\), \(d\)]\((\[CapitalGamma][i, j, k] ConT[j, k] + \[CapitalGamma][j, k, j] ConT[i, k])\)\)\)],{i,1,d}]*)


(* ::Input:: *)
(*(* SUMMARY *)*)


(* ::Input:: *)
(*eq11=EQ[1,1]//fsp;*)
(*eq22=EQ[2,2]//fsp;*)
(*eq33=EQ[3,3]//fsp;*)
(*eq44=cl[MEQ[1]//fsp,Derivative[1][A0][r]];*)
(*eq55=DT[2]//fsp;*)
