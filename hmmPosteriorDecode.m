(* Wolfram Language package *)

(* posteriorDecode should return the state names for the sequence of most likely state.*)
posteriorDecode[observationSeq_, hmm_] :=
	Module[{result, posteriorMatrix, i},
		posteriorMatrix = posteriorProbabilities[observationSeq,hmm];
		(*Map the result to hmm*)
		result = Table[hmm["states"][[Ordering[posteriorMatrix[[i]],-1][[1]]]],{i,Length[observationSeq]}];
		result
			
]
		
		
		 
posteriorProbabilities[observationSeq_, hmm_] := 
	Module[{forwardMatrix, backwardMatrix, posteriorMatrix, i},
		forwardMatrix = buildForwardMatrix[observationSeq, hmm];
		backwardMatrix = buildForwardMatrix[observationSeq, hmm];
		posteriorMatrix = forwardMatrix * backwardMatrix;
		
		Do[
			norm[posteriorMatrix[[i]]],
			{i,Length[observationSeq]}
		];
		
		posteriorMatrix
] 

 
 
buildForwardMatrix[observationSeq_, hmm_] := 
	Module[{numberOfObservations = Length[observationSeq], numberOfStates = Length[hmm["states"]], 
		forwardMatrix, observationIndex,i,j,k, prob,
		nO, nS, fM, oI, oS},
	  (* Put your code *)
	  nO = numberOfObservations;
	  nS = numberOfStates;
	  forwardMatrix = Table[0,{nO},{nS}];
	  fM = forwardMatrix;
	  oI = observationIndex;
	  oS = observationSeq;
	  
	  Do[
	  	fM[[1,i]] = hmm["emissionMatrix"][[oS[[1]],i]] * hmm["initialStateProbs"][[i]];
		,{i,nS}
		];
		 fM[[1]] = norm[fM[[1]]];
	 
		Do[
			Do[
				prob = Table[fM[[j-1,k]] * hmm["transitionMatrix"][[k,i]],{k,nS}];
				oI = oS[[j]];
				fM[[j,i]] = Total[hmm["emissionMatrix"][[oI,i]] * prob];
			
				,{i,nS}
			];
			
			fM[[j]] = norm[fM[[j]]];
		
			,{j,Range[2,nO]}
		]; 
	  
	  
	  
	 (* Return the Viterbi matrix. *)
	 fM] 

buildBackwardMatrix[observationSeq_, hmm_] := 
	Module[{numberOfObservations = Length[observationSeq], numberOfStates = Length[hmm["states"]], 
		backwardMatrix, observationIndex,
		i, j, k, prob, nO, nS, bM, oI, oS},
		nO = numberOfObservations;
		nS = numberOfStates;
		bM = backwardMatrix;
		oI = observationIndex;
		oS = observationSeq;
		
		(* Put your code for bulding the Viterbi matrix here. To give you an idea of what to expect,
         my code is 13 lines. But use as many lines as you need to make your code clear and readable. *)
		
		bM = Table[0, {nO}, {nS}];
		
		Do[
			bM[[nO, i]] =  1./nS;
		,{i, nS}
		];
		
		Do[
			Do[
				oI = oS[[j+1]];
				prob = Table[ hmm["emissionMatrix"][[oI,k]] * hmm["transitionMatrix"][[i,k]] * bM[[j+1,k]]
					,{k, nS}];
				
				bM[[j,i]] = Total[prob];
				
				,{i, nS}
			];
			
			bM[[j]] = norm[bM[[j]]];
			
			,{j, nO-1, 1, -1}
		];
		
		


	 (* Return the Viterbi matrix. *)
	 bM] 
 	 
 	 
norm[vb_] := 
	Module[{},
		If[Total[vb]==0,vb/Total[vb],Normalize[vb,Total]]
	]
	
	
	
	
	
	
	
	
	
	
	
	
