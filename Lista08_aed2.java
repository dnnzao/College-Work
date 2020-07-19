import java.util.Scanner;
import java.util.*;
import java.io.*;

public class Lista08_aed2 {

    /*
     * Questão 1: 
     */
    public double CalcularH (int nTermos) {
        double resultado=0.0;
        double num;
        for (int i=2; i <= nTermos+2; i++) {
        	num=i+3;
        	resultado = resultado + num/100;
        }
        return resultado;
    }
    
    /*
     * Questão 2: 
     */
    public int CalcularFatorial (int fat) {
        if (fat == 0) return 1;
        if (fat == 1) return fat;
        return (fat*CalcularFatorial(fat-1));
    }
    
    /*
     * Questão 3: 
     */
    public double CalcularMedia (int idade) {
        if (idade < 0) {
            System.out.println("Fim da função CalcularMedia().");
            return idade;
        }
        if (idade > 90) {
            System.out.println("Idade acima de 90 - não computada.");
            return CalcularMedia(idade);
        }
        
        System.out.println("Digite uma idade entre 0 e 90.");
        idade = sc.readInt();
        
    }
    
    /*
     * Questão 4: 
     */
    public double CalcularH2 (int nTermos) {
        double resultado=0;
        for (int i=0; i < nTermos; i++) {
            resultado = (1/(100-i)) + resultado;
        }
        return resultado;
    }

    /*
     * Questão 5: 
     */
    public int CalcularPotencia(int a, int b) {
    	if (b==0) return 1;
        if (b==1) return a;
        return (a * CalcularPotencia(a,b-1));
    }
    
    /*
     * Questão 6: 
     * Método para ler notas dos alunos.
     * Calcular média dos alunos reprovados (nota menor que 60)
     * Calcular amplitude dos alunos aprovados (maior nota entre os aprovados, menos a menor nota dos aprovados)
     * @return: void
     * @parm: null
     */
    public void LerNotas () {
        Scanner sc = new Scanner (System.in);
        double nota;
        double mediaReprovados;
        double somaReprovados = 0;
        double amplitudeAprovados;
        double maiorAprovado = 0;
        double menorAprovado = 101;
        int contaReprovados = 0;
        
        System.out.println("Digite as notas ou um valor negativo para sair do programa: ");
        nota = sc.nextDouble();
        
        while (nota >= 0) {
            if (nota>100) {
                System.out.println("ERRO: Nota acima de 100. Nota não registrada.");
            }
            if (nota < 60) {
                System.out.println("Aluno reprovado. Nota: " + nota);
                somaReprovados = somaReprovados + nota;
                contaReprovados++;
            }
            if (nota >= 60 && nota <= 100) {
                if (nota>maiorAprovado) maiorAprovado=nota;
                if (nota<menorAprovado) menorAprovado=nota;
            }
            System.out.println("Digite as notas ou um valor negativo para sair do programa: ");
            nota = sc.nextDouble();
        }
        
        mediaReprovados = somaReprovados/contaReprovados;
        amplitudeAprovados = maiorAprovado - menorAprovado;
        
        System.out.println(""+mediaReprovados);
        System.out.println(""+amplitudeAprovados);
    }
    
    /*
     * Questão 7: 
     */
	public double CalcularH (int numerador, int denominador, int nTermos) {
        if (denominador == nTermos) return numerador/denominador;
        return ((numerador/denominador)+CalcularH(numerador+2, denominador+1, nTermos));
        /*double soma=0;
        for ( ; denominador <= nTermos ; denominador++) {
        	soma = soma + (numerador/denominador);
        	numerador=numerador+2;
        }
        return soma;*/
    }
    
    /*
     * Questão 8: 
     */
    public void SomarIntervalo (int a, int b) {
        int soma=0;
        if (a > b) {
            for (int i=b; i<=a ; i++) soma = i + soma;
        	System.out.println("A soma do intervalo de " + a + " até " + b + 
        	" é igual a: " + soma);
        } else {
            for (int i=b; i>=a ; i--) soma = i + soma;
    		System.out.println("A soma do intervalo de " + a + " até " + b + 
    		" é igual a: " + soma);
        }
    }
    
/*---------------- ÁREA DE INPUT DE DADOS VIA TECLADO ----------------*/
	public void LerInteirosPositivos () {
        Scanner sc = new Scanner (System.in);
        int a=-1, 
        	b=-1;
        
        try {
            System.out.println("Digite um valor inteiro positivo: ");
            a = sc.nextInt();
            while (a<0) {
                System.out.println("ERRO: Valor negativo.");
                System.out.println("Digite um valor inteiro positivo: ");
                a = sc.nextInt();
            }
            System.out.println("Digite outro valor inteiro positivo: ");
            b = sc.nextInt();
            while (b<0) {
                System.out.println("ERRO: Valor negativo.");
                System.out.println("Digite um valor inteiro positivo: ");
                b = sc.nextInt();
            }
        } catch (NumberFormatException NFE ) {
            System.out.println("ERRO: NumberFormarException.");
            LerInteirosPositivos();
        } catch (NoSuchElementException NSEE) {
        	System.out.println("ERRO: NoSuchElementException.");
        	LerInteirosPositivos();
        } finally {
        	System.out.println("Números digitados: ");
        	System.out.println("A: " + a);
        	System.out.println("B: " + b);
        }
    }
    
    public void LerInteiros () {
        int a, b;
        Scanner sc = new Scanner (System.in);
        try {
            System.out.println("Digite o valor de A: ");
            a = sc.nextInt();
            System.out.println("Digite o valor de B: ");
            b = sc.nextInt();
        } catch (NumberFormatException e) {
            System.out.println("ERRO: Valores inválidos");
            LerInteiros();
        } catch (NoSuchElementException NSEE) {
        	System.out.println("ERRO: NoSuchElementException.");
        	LerInteiros();
        } finally {
        	System.out.println("Números digitados: ");
        	System.out.println("A: " + a);
        	System.out.println("B: " + b);
        }
    }
    
    public void LerValoresReais () {
        double a = -1, 
        	   b = -1;
        Scanner sc = new Scanner (System.in);
        try {
            System.out.println("Digite o valor de A: ");
            a = sc.nextDouble();
            System.out.println("Digite o valor de B: ");
            b = sc.nextDouble();
        } catch (NumberFormatException e) {
            System.out.println("ERRO: Valores inválidos");
            LerValoresReais();
        } catch (NoSuchElementException NSEE) {
        	System.out.println("ERRO: NoSuchElementException.");
        	LerValoresReais();
        }
        System.out.println("Números digitados: ");
        System.out.println("A: " + a);
        System.out.println("B: " + b);
    }
}