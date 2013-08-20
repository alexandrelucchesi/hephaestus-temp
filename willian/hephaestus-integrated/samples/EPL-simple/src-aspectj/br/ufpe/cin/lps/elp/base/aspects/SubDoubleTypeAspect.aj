package br.ufpe.cin.lps.elp.base.aspects;

import br.ufpe.cin.lps.elp.base.expression.*;

public aspect SubDoubleTypeAspect extends DoubleTypeAspect{
	
	pointcut expType(Expression e) :  if(e instanceof SubExp)&& target(e);
			
	DoubleValue around(Expression e) : callEval(e){
		SubExp ae = (SubExp)e;
		return new DoubleValue(
				((DoubleValue)ae.getLeft().evaluate()).value() -
				( (DoubleValue)ae.getRight().evaluate()).value() 
		);
	}
	
	

}
