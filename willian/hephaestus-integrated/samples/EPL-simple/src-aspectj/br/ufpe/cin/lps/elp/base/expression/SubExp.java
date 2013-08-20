package br.ufpe.cin.lps.elp.base.expression;

public class SubExp extends BinaryExp {

	public SubExp(Expression esq, Expression dir) {
		super(esq, dir, "-");
	}

	public Value evaluate() {
		Value leftValue = getLeft().evaluate();
		Value rightValue = getRight().evaluate();
		
		return new IntegerValue(((IntegerValue) leftValue).value()
					- ((IntegerValue) rightValue).value());
	}

}
