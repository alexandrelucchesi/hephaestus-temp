package br.ufpe.cin.lps.elp.base.aspects;

import br.ufpe.cin.lps.elp.base.expression.*;

public aspect AddDoubleTypeAspect extends DoubleTypeAspect{
	
	pointcut expType(Expression e) :  if(e instanceof AddExp) && target(e);	
	 
	DoubleValue around(Expression e) : callEval(e){
		AddExp ae = (AddExp)e;
		return new DoubleValue(
				((DoubleValue)ae.getLeft().evaluate()).value() +
				( (DoubleValue)ae.getRight().evaluate()).value() );
	}

}
