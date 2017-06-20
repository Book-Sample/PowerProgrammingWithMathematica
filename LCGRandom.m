(* ::Package:: *)

BeginPackage["Power`LCGRandom`"]
LCGSetSeed::usage="LCGSetSeed[x] sets the LCG random number generator's seed to the integer x."
LCGRandom::usage="LCGRandom[] generates a uniformly distributed random number in the range {0.,1.} using a linear congruential generator."
Begin["`Private`"]
modulus=2^31-1;
seed=1;
LCGSetSeed[x_Integer/;1<=seed<modulus]:=seed=x;
LCGRandom[]:=(seed=Mod[7^5*seed,modulus];N[seed/modulus]);
End[]
SetAttributes[{LCGSetSeed,LCGRandom},{Protected,ReadProtected}]
EndPackage[]
