package br.ufpe.cin.lps.elp.base.aspects;

//import br.ufpe.cin.lps.elp.base.parser.Parser;
import br.ufpe.cin.lps.elp.base.expression.Expression;
import br.ufpe.cin.lps.elp.base.expression.DoubleValue;
import br.ufpe.cin.lps.elp.base.expression.Value;

public abstract privileged aspect DoubleTypeAspect {
	
//	pointcut addValue(Parser p, String value): 
//		call (public void Parser.addValue(..)) && args(value) && target(p);
//	
//	void around(Parser p, String value): addValue(p,value) {
//		p.exp.add(new DoubleValue(Double.parseDouble(value)));
//	}
	
	abstract pointcut expType(Expression e); 
	
	pointcut callEval(Expression e) : call (public Value Expression.evaluate(..)) && expType(e);

}
