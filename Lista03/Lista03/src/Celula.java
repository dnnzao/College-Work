public class Celula {
	Produtos item;
	Celula prox;
	
	Celula() {
		this.item = new Produtos();
		this.prox = null;
	}
	
	Celula(Produtos item, Celula prox) {
		this.item=item;
		this.prox=prox;
	}

	public Produtos getItem() {
		return item;
	}

	public void setItem(Produtos item) {
		this.item = item;
	}

	public Celula getProx() {
		return prox;
	}

	public void setProx(Celula prox) {
		this.prox = prox;
	}
}
