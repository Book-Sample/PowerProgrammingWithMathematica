Null;
(*
   :Title: traceUtils

   :Author: Christopher Stover, July 1995 
            crstover@aol.com 

   :Copyright: Copyright 1995

   :Context: traceUtils`

   :Summary: This is a Mathematica package defining the functions 
tracePrintIndexed and tracePrintStack.  

tracePrintIndexed is an enhancement of TracePrint that, among other things, 
o  allows not printing steps that belong to trivial evaluation sub-chains,
o  indicates which steps begin new sub-chains and which replace previous
   steps, 
o  provides step numbers that can be used to "zoom in" on a portion of a
   shallow trace, and 
o  offers options similar to TraceOn and TraceOff that work inside of each
   other. 

tracePrintStack will print the evaluation stack as well as the current step at 

every step of the trace, or at one or more selected steps. 

After this package is loaded, usage messages may be displayed by typing 
?tracePrintIndexed, ?tracePrintStack, or ?(option) where option is one of the 
named options for these functions. A companion notebook, traceUExamples.ma, 
illustrates the features of these functions in more detail. 

   :Mathematica Version: 2.2

   :Limitations: If the count of steps that are assigned step numbers by 
tracePrintIndexed but not printed gets into the hundreds, the delay while these 

steps are being processed becomes noticable.
*)
BeginPackage["traceUtils`"];
(*
*)
Function[Unprotect[##]; ClearAll[##]][
  $step, (* internal vbl for tracePrintIndexed *)
  $stepNumber, (* internal vbl for tracePrintIndexed *)
  $traceLevel, (* internal vbl for tracePrintIndexed *)
  doubleSpace, (* option for tracePrintIndexed *)
  indentOutput, (* option for tracePrintIndexed *)
  printCondition, (* option for tracePrintIndexed and tracePrintStack *)
  skipTrivialChains, (* option for tracePrintIndexed *)
  tracePrintIndexed, 
  tracePrintStack,
  suppressForm, (* option for tracePrintIndexed *)
  suppressWhole, (* option for tracePrintIndexed *)
  unsuppressForm, (* option for tracePrintIndexed *)
  useCompleteStack (* option for tracePrintStack *)
];
(*
*)
doubleSpace::usage = "doubleSpace is an option for tracePrintIndexed.  The 
default value is False.  With doubleSpace -> True, a blank line is printed 
between each step.  This may improve readability when powers or quotients appear 
in the evaluation being traced.";
(*
*)
indentOutput::usage = "indentOutput is an option for tracePrintIndexed.  The 
default value is True.  With indentOutput -> True, each step printed by 
tracePrintIndexed is indented according to its trace level as is done by 
TracePrint.  With indentOutput -> False, the numerical trace level is printed 
after the step number and before the delimiter \":\" or \"$,\" and the step is 
not indented.";
(*
*)
printCondition::usage = "printCondition is an option for tracePrintIndexed and 
tracePrintStack. The default value is True.  Setting \"printCondition -> cond\" 
tells tracePrintIndexed to print a step only if \"cond\" evaluates to True.  The 
condition \"cond\" may involve the special symbols $step, $stepNumber, and 
$traceLevel, which represent the current step, its number, and its trace 
level.";
(*
*)
skipTrivialChains::usage = "skipTrivialChains is an option for 
tracePrintIndexed. The default value is True. When skipTrivialChains is assigned 
the value True, steps belonging to trivial chains in the evaluation are not 
printed. To print all steps, set skipTrivialChains -> False. ";
(*
*)
suppressForm::usage = "suppressForm is an option for tracePrintIndexed.  The 
default value is None.  When tracePrintIndexed encounters an expression matching 
suppressForm, printing of intermediate steps in the evaluation chain of that 
expression is suppressed.  This suppression will be temporarily removed within 
the evaluation chain of an expression matching unsuppressForm (q.v.). Compare 
suppressWhole";
(*
*)
suppressWhole::usage = "suppressWhole is an option for tracePrintIndexed. The 
default value is False. Suppression of step printing is initially turned on if 
suppressWhole is assigned the value True. With suppressWhole -> True, steps will 
be printed only within the evaluation chains of expressions matching 
unsuppressForm (q.v.). Compare suppressForm";
(*
*)
tracePrintIndexed::usage = "Typing tracePrintIndexed[expr, args] in place of 
TracePrint[expr, args] results in similar output but with a step number of the 
form \"nnn:\" or \"nnn$\" printed before each step.  The form \"nnn:\" is used 
if the step begins an evaluation subchain; the form \"nnn$\" is used if the step 
replaces the most recent step on the same trace level.
\n
tracePrintIndexed works by calling TraceScan. Optional arguments recognized by 
TraceScan may be given to tracePrintIndexed, except that the option TraceOn is 
disabled. Changing the optional arguments passed to TraceScan will normally 
change the assignment of step numbers.
\n
tracePrintIndexed has additional options named doubleSpace, indentOutput, 
printCondition, skipTrivialChains, suppressForm, suppressWhole, and 
unsuppressForm (q.v.). These options control what is printed without affecting 
the assignment of step numbers. 
\n
tracePrintIndexed assumes that the evaluation being traced does not alter the 
behavior of the evaluation stack (e.g. by calling StackBegin, StackComplete, or 
StackInhibit).";
(*
*)
tracePrintStack::usage = "tracePrintStack[expr] is like tracePrintIndexed[expr] 
except that the evaluation stack as well as the current expression is printed at 
each step. The format for printing steps is the same as used by 
tracePrintIndexed with indentOutput -> False.
\n
tracePrintStack recognizes options named printCondition and useCompleteStack 
(q.v.). Other optional arguments will be ignored.
\n
tracePrintStack assumes that the evaluation being traced does not alter the 
behavior of the evaluation stack (e.g. by calling StackBegin, StackComplete, or 
StackInhibit).";
(*
*)
unsuppressForm::usage = "unsuppressForm is an option for tracePrintIndexed.  The 
default value is None.  When suppression of step printing has been turned on by 
suppressForm (q.v.) or suppressWhole (q.v.), it is turned off again within the 
evaluation chains of expressions matching unsuppressForm.  Any number of nested 
suppressions and unsuppressions is possible.";
(*
*)
useCompleteStack::usage = "useCompleteStack is an option for tracePrintStack. 
The default value is False.  If set to True, StackComplete is effectively called 
during tracing. "; 
(*
*)
tracePrintIndexed::noTraceOn = "The option TraceOn does not work with 
tracePrintIndexed. Use unsuppressForm with suppressWhole -> True 
instead.";
(*
*)
Begin["`Private`"];
(*
*)
flattenRule = 
  RuleDelayed[
    Literal[CompoundExpression[a___, CompoundExpression[b___], c___]], 
    CompoundExpression[a, b, c] ];
(*
*)
SetAttributes[tracePrintIndexed, HoldAll];

Options[tracePrintIndexed] = 
  {doubleSpace -> False, 
   indentOutput -> True,
   printCondition -> True,
   skipTrivialChains -> True, 
   suppressForm -> None,
   suppressWhole -> False, 
   unsuppressForm -> None};
(*
*)
ReleaseHold[ReplaceRepeated[  (* inline substitution *)
Hold[
tracePrintIndexed[expr_, args___] :=
  Block[
    {(* public *)
      Continuation, LineBreak, $step, $stepNumber, $traceLevel,
     (* private *)
      localOptions, indOutpt, prntCond, skipTriv, supForm, supWhole, 
      unsupForm, printOneLine, doPrinting, presuppressPrint}, 
    setLocalOptions;
    Format[LineBreak[_]] = "";
    Format[Continuation[_]] = If[indOutpt, "    ", "       "];
    If[ (* TraceOn resets TraceLevel[] to 0 each time it matches something.
           This wrecks our whole approach. *)
      Positive[Count[Hold[args], (Rule | RuleDelayed)[TraceOn, _], {1}]],
      Message[tracePrintIndexed::noTraceOn];
      Break[]]; 
    setPrintOneLine;
    setPrintCommands;
    If[ (* Choose the head tracePIthin or tracePIfat *)
      supWhole === False && supForm === None,
      tracePIthin, (* no suppression *)
      tracePIfat
		    ][expr, (* will be Held *)
		      DeleteCases[ (* remove local Options from Hold[args] *)
		        Hold[args],  
		        (Rule | RuleDelayed)[ 
		          (doubleSpace | indentOutput | 
		           printCondition | skipTrivialChains | 
		           suppressForm | suppressWhole | unsuppressForm), _],
		        {1}] ]
  ];
], 

{
setLocalOptions :> 
  ( localOptions = 
      Cases[ 
        Cases[
          Hold[args],
          Literal[(Rule | RuleDelayed)[xx__]] :> RuleDelayed[xx]],
        Literal[
          _[ ( doubleSpace | indentOutput | 
               printCondition | skipTrivialChains | 
               suppressWhole | suppressForm | unsuppressForm ), 
             _]] ];
    {dblSpace, indOutpt, skipTriv, supWhole, supForm, unsupForm} = 
      {doubleSpace, indentOutput, skipTrivialChains, suppressWhole, 
        HoldForm[suppressForm], HoldForm[unsuppressForm]} 
           /. localOptions 
           /. Options[tracePrintIndexed];
    {supForm, unsupForm} = 
           Replace[#, HoldForm[None] -> None]& /@ {supForm, unsupForm};
    ReleaseHold[
      Hold[prntCond := printCondition] 
        /. localOptions 
        /. Options[tracePrintIndexed] ]
  ),

setPrintOneLine :> 
  ( If[indOutpt, 
      (*then*)
		      printOneLine := 
						  		Print[
						      PaddedForm[$stepNumber, 3, NumberSigns -> {"", ""}], 
						      delimString, 
						      Indent[-1 + $traceLevel], (* root step has TraceLevel 2 *)
						      $step ], 
      (*else*)
		      printOneLine := 
						    Print[
						      PaddedForm[$stepNumber, 3, NumberSigns -> {"", ""}], 
						      " ", 
						      PaddedForm[$traceLevel, 2, NumberSigns -> {"", ""}],
						      delimString, 
						      " ", 
						      $step ] ];
				If[dblSpace === True, 
				  ReleaseHold[
				    Hold[SetDelayed][
				      Hold[printOneLine],
				      Hold[Print[""]; printOneLine] 
				        /. OwnValues[printOneLine] ]]];		  
				If[OwnValues[prntCond] =!= {Literal[prntCond] :> True}, 
				  ReleaseHold[
				    Hold[SetDelayed][
						    Hold[printOneLine], 
						    Hold[If[prntCond, printOneLine]] /. OwnValues[printOneLine]]]]
  ),

setPrintCommands :>
		If[skipTriv,
		  (*then*)
		    doPrinting := 
							    If[thisTraceLevel =!= stackTopTraceLevel,
									    (*then*)
									      minUnprintedLevel = Min[minUnprintedLevel, thisTraceLevel],
									    (*else*)
									      printBackSteps;
											    delimString = "$"; 
											    {$step, $stepNumber, $traceLevel} = 
											      {thisStep, thisStepNumber, thisTraceLevel};
														 printOneLine;
											    minUnprintedLevel = Infinity ];
							  presuppressPrint := 
							    ( printBackSteps;      
					        delimString = 
					          If[thisTraceLevel === stackTopTraceLevel, "$", ":"];
									    {$step, $stepNumber, $traceLevel} = 
									      {thisStep, thisStepNumber, thisTraceLevel};
												 printOneLine;
									    minUnprintedLevel = Infinity ),
		  (*else*) 
		    doPrinting := 
        ( delimString = 
            If[thisTraceLevel === stackTopTraceLevel, "$", ":"];
								  {$step, $stepNumber, $traceLevel} = 
								      {thisStep, thisStepNumber, thisTraceLevel};
								  printOneLine );
						presuppressPrint := doPrinting
		],

printBackSteps :>
  If[minUnprintedLevel =!= Infinity, 
		  delimString = ":";
		  Do[
		    {$step, $stepNumber, $traceLevel} = 
		      {lastStepFn[i], 
		        lastStepNumFn[i], i};
		    printOneLine,
		  {i, minUnprintedLevel, stackTopTraceLevel}] ],

flattenRule}
]];
(*
*)
SetAttributes[tracePIthin, HoldFirst]; 

tracePIthin[expr_, Hold[args___]] :=
  Block[
    {thisStep, thisStepNumber, thisTraceLevel, 
      stackTopTraceLevel, lastStepFn, lastStepNumFn,
      minUnprintedLevel, delimString},   
    thisStepNumber = 1;
    minUnprintedLevel = Infinity;
    StackBegin[TraceScan[ 
		    Function[StackInhibit[
		         (* First[Stack[_]] in here would give this Function, and 
		            Rest[Stack[_]] the current Stack for evaluating expr. *)
		      thisStep = #;
		      thisTraceLevel = TraceLevel[]; (* gives 2 at Step 1 *)
		      stackTopTraceLevel = Length[Stack[]]; (* gives 1 at Step 1 *)
		      doPrinting;
		      lastStepFn[thisTraceLevel] = thisStep;
		      lastStepNumFn[thisTraceLevel] = thisStepNumber;
		      thisStepNumber++
		    ]], 
		    expr, args
    ]];
    If[thisStepNumber =!= 1,
      ReleaseHold[lastStepFn[2]] ] (* 2 was the TraceLevel of the first step *)

  ];
(*
*)
SetAttributes[tracePIfat, HoldFirst];

ReleaseHold[ReplaceRepeated[ 
Hold[
tracePIfat[expr_, Hold[args___]] :=
  Block[
      {thisStep, thisStepNumber, thisTraceLevel, thisStack, 
      stackTopTraceLevel, nextTraceLevel, nextStack,
      nextStackTopTraceLevel,
      suppressedQ, catchLevel, catchLevelStack, lastCatchLevel, 
      lastStepFn, lastStepNumFn, lastSuppressedStepNumFn, 
      minUnprintedLevel, delimString}, 
    catchLevel = DirectedInfinity[-1];
    catchLevelStack = {};    
    suppressedQ = supWhole;
    thisStepNumber = 0;
    minUnprintedLevel = Infinity;
    StackBegin[TraceScan[ 
		    Function[StackInhibit[
		      nextTraceLevel = TraceLevel[]; (*We work one step behind the Scan*)
		      nextStackTopTraceLevel = Length[Stack[]];
		      If[thisStepNumber =!= 0, 
				      If[suppressedQ,
				        catchUnsuppression,
				        catchSuppression];              
				      If[!suppressedQ, doPrinting];
				      lastStepFn[thisTraceLevel] = thisStep;
				      lastStepNumFn[thisTraceLevel] = thisStepNumber;
				      uncatch];
		      thisStep = #;
		      thisStepNumber++;
		      {thisTraceLevel, stackTopTraceLevel} = 
		        {nextTraceLevel, nextStackTopTraceLevel}
		    ]],
		    expr, args
		  ]];
		  If[thisStepNumber =!= 0,
				  If[!suppressedQ, doPrinting]; (* final step *)
				  lastStepFn[thisTraceLevel] = thisStep;
				  lastStepNumFn[thisTraceLevel] = thisStepNumber;
				  nextStackTopTraceLevel = DirectedInfinity[-1];
				  uncatch; (* may print some final members of suppressed chains *)
		    ReleaseHold[lastStepFn[2]] ]
  ]
], 
{     
catchUnsuppression :>
  If[MatchQ[thisStep, unsupForm], 
    suppressedQ = False;
    catchLevelStack = {catchLevel, catchLevelStack};
    catchLevel = thisTraceLevel],

catchSuppression :> 
	 If[MatchQ[thisStep, supForm] && 
	      nextStackTopTraceLevel === thisTraceLevel,  
	        (* don't suppress one-item evaluation chains *)
	    presuppressPrint;
	    lastSuppressedStepNumFn[thisTraceLevel] = thisStepNumber;
	    suppressedQ = True;
	    catchLevelStack = {catchLevel, catchLevelStack};
	    catchLevel = thisTraceLevel],

uncatch :> 
  If[nextStackTopTraceLevel < catchLevel,
		  lastCatchLevel = Infinity; 
		  While[nextStackTopTraceLevel < catchLevel,
				  If[suppressedQ &&
				    catchLevel < lastCatchLevel &&
				    lastSuppressedStepNumFn[catchLevel] < lastStepNumFn[catchLevel], 
				  (* print final value of suppressed expr *)
				    {$step, $stepNumber, $traceLevel} = 
				      {lastStepFn[catchLevel], lastStepNumFn[catchLevel], catchLevel};
				    delimString = "$";          
								printOneLine];
		    suppressedQ = Not[suppressedQ];
		    lastCatchLevel = catchLevel;
		    {catchLevel, catchLevelStack} = catchLevelStack]],

flattenRule}
]];
(*
*)
SetAttributes[tracePrintStack, HoldAll];

Options[tracePrintStack] = 
  { printCondition -> True,
    useCompleteStack -> False};
  
tracePrintStack[expr_, args___] := 
  Block[
    { LineBreak, Continuation,
      $step, $stepNumber, $traceLevel, firstTraceLevel, delimStr, 
      thisStack, lenStack, lenLastStack, stackTop, lastStackTop, 
      stepNumList = Hold[], traceLevList = Hold[], lastTraceLev, 
      delimStrList = Hold[], lastDelimStr, 
      localOptions, prntCond, useComplStck},
    Format[LineBreak[_]] = "";
    Format[Continuation[_]] = "     ";       
    localOptions = 
      Cases[
        Hold[args], 
        (RuleDelayed | Rule)[ 
            (printCondition | useCompleteStack), _], 
        {1} ];
    useComplStck = useCompleteStack /. localOptions /. 
      Options[tracePrintStack];
    ReleaseHold[Hold[prntCond := printCondition] /. localOptions /. 
      Options[tracePrintStack] ];
    $stepNumber = 1; 
    lenLastStack = 0; 
    StackBegin[ If[useComplStck, StackComplete, Identity][
      TraceScan[ 
        Function[StackInhibit[
          $step = #;
          $traceLevel = TraceLevel[];
          If[$stepNumber === 1, firstTraceLevel = $traceLevel];
          thisStack = (*Remove spurious steps arising from this Function*)
            Drop[
              If[useComplStck, 
                Drop[Stack[_], -1],
                Stack[_]],
              2];
          lenStack = Length[thisStack];
          stackTop = If[lenStack === 0, None, Last[thisStack]];
          Which[
            lenStack < lenLastStack, 
              stepNumList = Take[stepNumList, lenStack];
              traceLevList = Take[traceLevList, lenStack];
              delimStrList = Take[delimStrList, lenStack],
            lenStack > lenLastStack,
              stepNumList = Append[stepNumList, -1 + $stepNumber];
              traceLevList = Append[traceLevList, lastTraceLev];
              delimStrList = Append[delimStrList, lastDelimStr],
            stackTop =!= lastStackTop, 
              stepNumList[[lenLastStack]] = -1 + $stepNumber;
              traceLevList[[lenLastStack]] = lastTraceLev;
              delimStrList[[lenLastStack]] = lastDelimStr];
          delimStr = 
            If[
              lenStack === 0 || $traceLevel =!= Last[traceLevList], 
              ":", 
              "$"];
          If[prntCond,
            Do[
										    Print[
										      PaddedForm[stepNumList[[i]], 3, NumberSigns -> {"", ""}], 
										      " ", 
										      PaddedForm[traceLevList[[i]], 2, NumberSigns -> {"", ""}],
										      delimStrList[[i]], 
										      " ", 
										      thisStack[[i]] ],
              {i, lenStack}];
            Print["------- stack/step ", $stepNumber, " -------"];
								    Print[
								      PaddedForm[$stepNumber, 3, NumberSigns -> {"", ""}], 
								      " ", 
								      PaddedForm[$traceLevel, 2, NumberSigns -> {"", ""}],
								      delimStr, 
								      " ", 
								      $step];
            Print[" "] ];
		        {lastTraceLev, lastDelimStr, lenLastStack, lastStackTop} = 
		          {$traceLevel, delimStr, lenStack, stackTop};
		        $stepNumber++]], (* end StackInhibit, Function *)
        expr]]]; (*end TraceScan, StackComplete/Identity, and StackBegin*)
				If[$stepNumber =!= 1, 
				  ReleaseHold[
		      If[$traceLevel === firstTraceLevel,
		        $step,
		        Part[
		          thisStack, 
		          Count[traceLevList, firstTraceLevel]] ] ] ]
		];
(*
*)
Protect[
  doubleSpace, 
  indentOutput,
  printCondition,
  stepNumberDigits,
  skipTrivialChains,
  suppressForm,
  suppressWhole,
  tracePrintIndexed, 
  tracePrintStack,
  unsuppressForm,
  useCompleteStack, 
  $step,
  $stepNumber,
  $traceLevel
];
(*
*)
End[];
EndPackage[];
