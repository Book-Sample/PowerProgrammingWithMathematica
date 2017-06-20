(* ::Package:: *)

BeginPackage["Power`Factorization`"]
Factorization::usage="\:4ee3\:8868\:4e00\:4e2a\:56e0\:5f0f\:5206\:89e3\:7684\:6570\:636e\:7ed3\:6784\:ff0c\:80fd\:591f\:8fdb\:884c\:52a0\:6cd5\:ff0c\:4e58\:6cd5\:548c\:4e58\:65b9\:8fd0\:7b97"
Begin["`Private`"]
(*define the FormatValues*)
formatrules={{a_,1}->SequenceForm[a," "],{a_,b_}->SequenceForm[Superscript[a,b]," "]};
Format[x_Factorization]:=Apply[SequenceForm,x/.formatrules]//Flatten//Drop[#,-1]&

(*Overrding FactorInteger\:51fd\:6570*)
wasProtected=Unprotect[FactorInteger];
intercept=True;
FactorInteger[x_Integer]/;intercept:=Block[{intercept=False},Factorization@@FactorInteger[x]]
Protect@@wasProtected;
Factorization[{p_,1}]:=p;
Factorization[head___,{x_/;x!=0,0},tail___]:=Factorization[head,{1,1},tail];
Factorization[head___,{1,x_/;x!=1(*x\:4e0d\:80fd\:4e3a1\:ff0c\:5426\:5219\:8fdb\:5165\:9012\:5f52\:7684\:6b7b\:5faa\:73af*)},tail___]:=Factorization[head,{1,1},tail];
Factorization[{1,1},rest__(*for Factorization is Oderless, it makes no difference whether the rest is on the left or right of {1,1}*)]:=Factorization[rest];

(*Expanding Factorizations*)
ExpandFactorization[x_Factorization]:=Times@@Apply[Power,x,{1}];
ExpandFactorization[x_]:=x;
ExpandAllFactorization[x_]:=MapAll[ExpandFactorization,x];

(*\:4e58\:6cd5\:89c4\:5219*)
Factorization/:x_Factorization*y_Factorization:=Join[x,y];
SetAttributes[Factorization,Orderless];
Factorization[head___,{a_,b_},{a_,c_},tail___]:=Factorization[head,{a,b+c},tail];
Factorization/:Times[x_Factorization,y_Integer/;y!=0]:=Factorization@@Join[List@@x,Block[{intercept=False},FactorInteger[y]]];
Factorization/:Times[f_Factorization,r_Rational/;r!=0]:=Factorization@@Join[List@@f,
Block[{intercept=False},FactorInteger[Numerator[r]]],
Replace[Block[{intercept=False},FactorInteger[Denominator[r]]],{b_,e_}:>{b,-e},{1}]
];
(*\:6307\:6570\:89c4\:5219*)
Factorization/:Power[a_Factorization,b_Integer|b_Rational]:=a/.{x_,y_}:>{x,y b};
(*\:52a0\:6cd5\:89c4\:5219*)
ExpandFactorization[x__Factorization]:=Sequence@@ExpandFactorization/@{x};
Factorization/:Plus[a__Factorization](*\:5728\:5b9a\:4e49\:7684\:5730\:65b9\:5c31\:52a0\:6761\:4ef6\:ff0c\:800c\:4e0d\:662f\:5728\:65b9\:6cd5\:4f53\:91cc\:9762\:52a0\:6761\:4ef6\:ff0cjava\:7c7b\:4f3c\:7684\:529f\:80fd\:662f\:901a\:8fc7\:63a5\:53e3\:5b9a\:4e49*)/;Length[List[a]]>1(*\:8fd9\:91cc\:5207\:8bb0\:52a0\:4e0a\:53c2\:6570\:4e2a\:6570\:5927\:4e8e1\:7684\:6761\:4ef6\:ff0c\:5426\:5219\:ff0cExpandFactorization\:5c06\:5339\:914d\:4e00\:4e2a\:53c2\:6570\:7684\:89c4\:5219\:ff0c\:9677\:5165Factorization\:901a\:8fc7ExpandFactorization\:8f6c\:4e3aInteger\:ff0cInteger\:901a\:8fc7FactorInteger\:8f6c\:4e3aFactorization\:7684\:6b7b\:5faa\:73af\:ff0c\:5728\:8fd9\:4e2a\:5faa\:73af\:91cc\:9762\:8017\:4e86\:4e00\:4e2a\:534a\:5c0f\:65f6*):=FactorInteger[Plus[ExpandFactorization[a]]];
Factorization/:Plus[a_Integer,b__Factorization]:=FactorInteger[Plus[a,ExpandFactorization[b]]];
(*\:6570\:503c\:503c*)
N[a_Factorization,_]:=ExpandFactorization[a]
End[]
SetAttributes[Factorization,{Protected,ReadProtected}]
EndPackage[]



