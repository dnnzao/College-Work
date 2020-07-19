public class Lista {
	private Celula cabeca, cauda;
	
	Lista(){
		this.cabeca = new Celula();
		this.cauda = this.cabeca;
	}
	
	public void Inserir(Produtos produto) {
		this.cauda.prox = new Celula(produto, null);
		this.cauda = this.cauda.prox;
	}
	
	public Celula getCabeca() {
		return cabeca;
	}

	public void setCabeca(Celula cabeca) {
		this.cabeca = cabeca;
	}

	public Celula getCauda() {
		return cauda;
	}

	public void setCauda(Celula cauda) {
		this.cauda = cauda;
	}
}
