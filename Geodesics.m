(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
<<preamble.m
Do[g[i,j]=gg[[i,j]]/.Table[xx[[i]]->xxVor[[i]],{i,1,d}],{i,1,d},{j,1,d}]
TT=Sum[g[i,j]D[xxVor[[i]],\[Tau]]D[xxVor[[j]],\[Tau]],{i,1,d},{j,1,d}];
Geo1=Table[D[D[TT,D[xxVor[[i]],\[Tau]]],\[Tau]]==D[TT,xxVor[[i]]],{i,1,d}];
eqs=ReplacePart[Geo1,2->TT==lagrangian];
Veff[\[Tau]_]=lagrangian-(TT-g[2,2]r'[\[Tau]]^2);
Veffder[\[Tau]_]=D[Veff[\[Tau]],r[\[Tau]]];
BHplot=ParametricPlot[{rs Cos[u],rs Sin[u]},{u,0,2 Pi}]/.Line[l_List]:>{{Black,Polygon[l]},{Directive[AbsoluteThickness[3],Black],Line[l]}};
InConds={t[0]==0,r[0]==rmax,\[Phi][0]==0,\[Theta][0]==Pi/2,Veff[0]==\[Gamma],\[Theta]'[0]==0,Veffder[0]==L};
sol=NDSolve[{eqs,InConds},xxVor,{\[Tau],\[Tau]min,\[Tau]max}];
solnumber=Intersection[Flatten[Position[t[\[Tau]]/.sol/.\[Tau]->\[Tau]max,_?(#>0&)]],
Flatten[Position[r[\[Tau]]/.sol/.\[Tau]->\[Tau]max,_?(Im[#]==0&)]],
Flatten[Position[r[\[Tau]]/.sol/.\[Tau]->\[Tau]max,_?(#<rmax&)]],
Flatten[Position[\[Phi][\[Tau]]/.sol/.\[Tau]->\[Tau]max,_?(#>\[Phi][\[Tau]]/.sol[[1]]/.\[Tau]->0&)]]][[1]];
