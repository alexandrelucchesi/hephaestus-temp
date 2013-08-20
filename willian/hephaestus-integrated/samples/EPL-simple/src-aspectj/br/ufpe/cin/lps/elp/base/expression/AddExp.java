package br.ufpe.cin.lps.elp.base.expression;

//Mecanismo de instanciacao no parser
public class AddExp extends BinaryExp {

	public AddExp(Expression esq, Expression dir) {
		super(esq, dir, "+");
	}

	public Value evaluate() {
		Value leftValue = getLeft().evaluate();
		Value rightValue = getRight().evaluate();

		return new IntegerValue(((IntegerValue) leftValue).value()
				+ ((IntegerValue) rightValue).value());
	}

}
