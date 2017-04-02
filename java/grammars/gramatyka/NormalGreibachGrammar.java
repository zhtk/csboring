package gramatyka;

import java.util.ArrayList;

/**
 * Klasa reprezentująca gramatykę Greibach
 * @author Piotr
 */
public class NormalGreibachGrammar extends ContextFreeGrammar {
    /**
     * Konstruktor gramatyki. Wywoływać tylko po sprawdzeniu że gramatyka jest
     * we właściwej postaci.
     * @param grammar Gramatyka z której ma zostać utworzony obiekt. Przed 
     * wywołaniem należy sprawdzić przy pomocy {@link ContextFreeGrammar#ifGreibach()}
     * czy argument jest w prawidłowej postaci
     */
    public NormalGreibachGrammar(ContextFreeGrammar grammar) {
        super(grammar);
        
        if(!ifGreibach())
            throw new InvalidArgumentsException("Gramatyka nie jest w postaci Greibach");
    }
    
    public NormalGreibachGrammar(NormalChomskyGrammar grammar) {
        super(grammar);
        
        int to = produkcje.length;
        while(!zasada1(to));
        zasada3();
    }
    
    /**
     * Funkcja wyszukująca numer indeksu symbolu nieterminalnego
     * @param c symbol zawarty w produkcji
     * @return numer indeksu symbolu nieterminalnego lub -1 jeśli symbol
     * nie jest nieterminalny
     */
    private int nonTerminalIndex(char c) {
        int i;
        
        for(i = 0; i < nonTerminale.length(); ++i)
            if(nonTerminale.charAt(i) == c)
                return i;
        
        return -1;
    }
    
    /**
     * Wyszukuje pierwszy wolny symbol nieterminalny
     * @return znaleziony znak
     */
    private char freeNonTerminal() {
        for(char i='A'; i<='Z'; ++i) 
            if(!nonTerminale.contains(String.valueOf(i)))
                return i;
        
        // Tu nic się nie da zrobić, a jeśli to nie jest błąd krytyczny
        // to można złapać ten wyjątek
        throw new RuntimeException("Internal grammar error");
    }
    
    private boolean zasada1(int to) {
        boolean done = true;
        
        for(int i = 0; i < to; ++i)
            for(int j = 0; j < produkcje[i].length; ++j) {
                String reg = produkcje[i][j];
                
                // Regułki się nie stosuje
                int nti = nonTerminalIndex(reg.charAt(0));
                if(nti == -1 || i <= nti)
                    continue;
                
                reg = produkcje[nti][0] + reg.charAt(1);
                produkcje[i][j] = reg;
                
                zasada2(i, j);
                
                done = false;
            }
        
        return done;
    }
    
    private void zasada2(int i, int j) {
        // Nie ma rekursji
        if(produkcje[i][j].length() == 0)
            return;
        
        char c = produkcje[i][j].charAt(0);
        if(nonTerminalIndex(c) != i)
            return;
                
        // Jest rekursja
        c = freeNonTerminal();
        nonTerminale += c;
                
        // Lista produkcji wszystkich symboli nieterminalnych 
        // z nowym symbolem
        String[][] p = new String[produkcje.length+1][];
        for(int k=0; k < produkcje.length; ++k)
            p[k] = produkcje[k];
                
        // Nowa lista produkcji dla symbolu występującego w rekursji
        ArrayList<String> l = new ArrayList<>();
                
        // Skopiowanie istniejących produkcji z wyjątkiem rekursywnej
        for(int k=0; k < produkcje[i].length; ++k)
            if(k != j)
                l.add(produkcje[i][k]);
                
        // Nowe produkcje z nowo wprowadzonym symbolem
        for(int k=0; k < produkcje[i].length; ++k)
            if(k != j)
                l.add(produkcje[i][k] + c);
        
        p[i] = new String[l.size()];
        p[i] = l.toArray(p[i]);
        
        // Dwie dodatkowe produkcje
        p[p.length-1] = new String[]{
                produkcje[i][j].substring(1),
                produkcje[i][j].substring(1) + c
            };
                
        // Końcówka
        produkcje = p;
    }
    
    private boolean isLineInGreibachForm(int line) {
        for(int i=0; i<produkcje[line].length; ++i)
            if(!isProductionInGreibachForm(line, i))
                return false;
        
        return true;
    }
    
    private void zasada3() {
        int todo = nonTerminale.length();
        boolean[] gotoweProdukcje = new boolean[todo];
        
        // Szukamy gotowej niewykorzystanej produkcji
        while(todo > 0) {
            for(int i=nonTerminale.length()-1; i>=0; --i)
                if(!gotoweProdukcje[i] && isLineInGreibachForm(i)) {
                    --todo;
                    gotoweProdukcje[i] = true;
                }
            
            // Zamiana pozostałych produkcji
            for(int j=0; j<produkcje.length; ++j) {
                ArrayList<String> l = new ArrayList<>();
                    
                for(int k=0; k<produkcje[j].length; ++k){
                    if(isProductionInGreibachForm(j, k)) {
                        l.add(produkcje[j][k]);
                        continue;
                    }
                    
                    char f = produkcje[j][k].charAt(0);
                    int fpos = nonTerminale.indexOf(f);
                        
                    if(gotoweProdukcje[fpos])
                        for(int i=0; i<produkcje[fpos].length; ++i)
                            l.add(produkcje[fpos][i] + produkcje[j][k].substring(1));
                    else
                        l.add(produkcje[j][k]);
                }
                
                produkcje[j] = l.toArray(produkcje[j]);
            }    
        }
    }
}
