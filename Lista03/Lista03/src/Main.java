import java.util.Scanner;
public class Main {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Lista lista1 = new Lista();
		Scanner sc = new Scanner(System.in);
		
		Produtos p1 = new Produtos(1, 10.0, 100);
		Produtos p2 = new Produtos(2, 20.0, 150);
		
		lista1.Inserir(p1);
		lista1.Inserir(p2);
	}

}
