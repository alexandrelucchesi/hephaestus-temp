package br.ufpe.cin.lps.elp.base.expression;

public abstract class BinaryExp implements Expression {

	private Expression left;

	private Expression right;

	private String operator;

	public BinaryExp(Expression left, Expression right, String operator) {
		this.left = left;
		this.right = right;
		this.operator = operator;
	}

	public Expression getLeft() {
		return left;
	}

	public Expression getRight() {
		return right;
	}

	public String getOperator() {
		return operator;
	}
	
	public void setLeft(Expression left) {
		this.left = left;
	}

	public void setRight(Expression right) {
		this.right = right;
	}

	@Override
	public String toString() {
		return String.format("%s %s %s", left, operator, right);
	}

}
