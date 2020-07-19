public class Fila {
	private int tamanho;
	private int fim;
	private int[] fila;
	
	Fila () {	
		this.tamanho=0;
		this.fim=0;
	}
	
	Fila (int tamanho) {
		this.tamanho=tamanho;
	}
	
	public boolean Enfileirar (int posicao) {
		if (posicao<=0 || posicao>this.fim) {
			System.out.println("LISTA CHEIA ORA ORA ORA ORA ORA!!!");
			return false;
		}
		for (int i=0; i<tamanho; i++) {
			if (i==posicao) {
				this.fila[posicao]=i;
				return true;
			}
		}
		return true;
	}
}
