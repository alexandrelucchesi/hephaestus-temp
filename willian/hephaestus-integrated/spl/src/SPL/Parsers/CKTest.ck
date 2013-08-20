configuration knowledge {
 A => selectScenario ( sc01, sc02 ), evaluateAdvice( adv01 ) ; 
 And (B, Not(C)) => selectScenario ( sc03 );
 Or (C, D) => evaluateAdvice( adv02 );
 Not( E ) => bindParameter(X,Y); 
}
