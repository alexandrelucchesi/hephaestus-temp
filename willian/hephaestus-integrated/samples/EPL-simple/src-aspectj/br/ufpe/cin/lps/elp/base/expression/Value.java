package br.ufpe.cin.lps.elp.base.expression;

public abstract class Value<T> implements Expression {

	private T value;

	public String toString() {
		return String.valueOf(value);
	}

	public Value(T value) {
		this.value = value;
	}

	public T value() {
		return value;
	}

	public boolean isEquals(Value<T> obj) {
		return value().equals(obj.value());

	}

	public Value<T> evaluate() {
		return this;
	}
	
	

}
