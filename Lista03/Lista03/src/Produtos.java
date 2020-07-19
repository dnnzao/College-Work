public class Produtos {
	private static final int NULL = 0;
	private int codigo, quantidade;
	private double preco;
	
	Produtos() {
		this.codigo = NULL;
		this.quantidade = NULL;
		this.preco=0.0;
	}
	
	Produtos (int codigo, double preco, int quantidade) {
		if (quantidade<=0) {
			System.out.println("Quantidade inválida!");
		}
		if (preco<=0) {
			System.out.println("Preço inválida!");
		}
		
		this.quantidade=quantidade;
		this.preco=preco;
		this.codigo=codigo+1;
	}
	
	public int getCodigo() {
		return codigo;
	}

	public void setCodigo(int codigo) {
		this.codigo = codigo;
	}

	public int getQuantidade() {
		return quantidade;
	}

	public void setQuantidade(int quantidade) {
		this.quantidade = quantidade;
	}

	public double getPreco() {
		return preco;
	}

	public void setPreco(double preco) {
		this.preco = preco;
	}

	double setDesconto (int desconto) {
		if (desconto<=0) {
			System.out.println("Desconto inválido!");
			return 0;
		}
		this.preco=preco-(preco/desconto);
		return preco;
	}
}