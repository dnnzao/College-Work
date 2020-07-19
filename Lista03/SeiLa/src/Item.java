public class Item {
	private int valor;
	private Item item;
	
	Item(){ 
		this.item=null;
	}
	
	Item (int valor) {
		this.valor=valor;
	}
	
	void setItem(int valor) {
		this.valor=valor;
	}
}
