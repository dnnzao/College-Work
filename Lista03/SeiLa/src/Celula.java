public class Celula {
	Item elemento;
	Celula prox;
	
	Celula(){ 
		this.elemento=null;
		this.prox=null;
	}
	
	Celula(Item elemento, Celula prox) {
		this.elemento=elemento;
		this.prox=prox;
	}
}
