package br.ufpe.cin.lps.elp.base;

import br.ufpe.cin.lps.elp.base.expression.Expression;
import br.ufpe.cin.lps.elp.base.expression.Value;

public class Program {

	private Expression exp;

	public Value executar() {
		Value result = exp.evaluate();
		//System.out.print(exp+" = ");
		System.out.println(result);
		return result;		
 	}

	public Program(Expression exp){
		this.exp = exp;
	}

	public Expression getExpressao() {
		return exp;
	}
}
