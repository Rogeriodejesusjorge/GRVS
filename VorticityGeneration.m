(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
<<preamble.m
<<Geodesics.m
toftautemp=Table[{\[Tau],t[\[Tau]]/.sol[[solnumber]]}/.\[Tau]->tt,{tt,\[Tau]min,\[Tau]max,(\[Tau]max-\[Tau]min)/nsteps}];
toftau=Interpolation[toftautemp,Method->"Spline"];
roftautemp=Table[{\[Tau],r[\[Tau]]/.sol[[solnumber]]}/.\[Tau]->tt,{tt,\[Tau]min,\[Tau]max,(\[Tau]max-\[Tau]min)/nsteps}];
roftau=Interpolation[roftautemp,Method->"Spline"];
rofttemp=Table[{t[\[Tau]]/.sol[[solnumber]],r[\[Tau]]/.sol[[solnumber]]}/.\[Tau]->tt,{tt,\[Tau]min,\[Tau]max,(\[Tau]max-\[Tau]min)/nsteps}];
roft=Interpolation[rofttemp,Method->"Spline"];
\[Theta]ofttemp=Table[{t[\[Tau]]/.sol[[solnumber]],\[Theta][\[Tau]]/.sol[[solnumber]]}/.\[Tau]->tt,{tt,\[Tau]min,\[Tau]max,(\[Tau]max-\[Tau]min)/nsteps}];
\[Theta]oft=Interpolation[\[Phi]ofttemp,Method->"Spline"];
\[Phi]ofttemp=Table[{t[\[Tau]]/.sol[[solnumber]],\[Phi][\[Tau]]/.sol[[solnumber]]}/.\[Tau]->tt,{tt,\[Tau]min,\[Tau]max,(\[Tau]max-\[Tau]min)/nsteps}];
\[Phi]oft=Interpolation[\[Phi]ofttemp,Method->"Spline"];
tmin=toftau[\[Tau]min];
tmax=toftau[\[Tau]max];
velocityvector[t_]={1,roft'[t],\[Theta]oft'[t],\[Phi]oft'[t]};
metricoft[t_]=gg/.r->roft[t]/.\[Theta]->\[Theta]oft[t]/.\[Phi]->\[Phi]oft[t];
\[CapitalGamma]oft[t_]=\[Sqrt](1/Sum[metricoft[t][[i,j]]velocityvector[t][[i]]velocityvector[t][[j]],{i,1,d},{j,1,d}]);
\[CapitalGamma]ofrtemp1=Table[{roftau[\[Tau]],toftau'[\[Tau]]},{\[Tau],\[Tau]min,\[Tau]max,(\[Tau]max-\[Tau]min)/nsteps}];
\[CapitalGamma]ofrtemp2=Table[{roft[t],\[CapitalGamma]oft[t]},{t,tmin,tmax,(tmax-tmin)/nsteps}];
\[CapitalGamma]ofr1=Interpolation[\[CapitalGamma]ofrtemp1,Method->"Spline"];
\[CapitalGamma]ofr2=Interpolation[Abs[\[CapitalGamma]ofrtemp2],Method->"Spline"];
\[CapitalOmega][r_]=(\[Chi]/q)/Sqrt[gg[[2,2]]gg[[4,4]]/.\[Theta]->\[Pi]/2] D[1/\[CapitalGamma]ofr1[r],r]temperature;
