package gramatyka;

/**
 * Klasa reprezentująca gramatykę bezkontekstową
 */
public class ContextFreeGrammar {
    /**
     * Niepusty zbiór symboli terminalnych (terminali)
     */
    protected String terminale;
    /**
     * Niepusty zbiór symboli nieterminalnych (nieterminali), pierwszy znak jest
     * symbolem początkowym (aksjomatem)
     */
    protected String nonTerminale;
    /**
     * Tablica tablic zawierających prawe strony produkcji zakodowane w postaci 
     * napisów; każdy wiersz tablicy zawiera zbiór prawych stron dla danego 
     * nieterminala (w kolejności określonej w tablicy {@link #nonTerminale})
     */
    protected String[][] produkcje;
    
    /**
     * Napis zawierający typ gramatyki w formacie zwracanym przez funkcję
     * {@link #getGrammarType()}
     */
    private String grammarForm;
    
    /**
     * Główny konstruktor gramatyki. Przyjmujemy, 
     * że nieterminale są zakodowane za pomocą dużych liter alfabetu 
     * angielskiego (A-Z), a terminale — za pomocą małych liter alfabetu 
     * angielskiego (a-z).
     * @param terminal napis reprezentujący symbole terminalne
     * @param nonTerminal napis reprezentujący symbole nieterminalne 
     *      (pierwsza litera napisu określa aksjomat (symbol poczatkowy) 
     *      gramatyki
     * @param prod  tablica tablic zawierających prawe strony produkcji 
     * zakodowane w postaci napisów; każdy wiersz tablicy zawiera zbiór 
     * prawych stron dla danego nieterminala
     * @throws gramatyka.InvalidArgumentsException Wyjątek rzucany gdy argumenty nie mają
     * sensu
     */
    public ContextFreeGrammar(String terminal, String nonTerminal, String[][] prod)
        throws InvalidArgumentsException
    {
        // Sprawdzenie sensowności argumentów
        if(!isTerminalCorrect(terminal))
            throw new InvalidArgumentsException("terminal niepoprawny");
        
        if(!isNonterminalCorrect(nonTerminal))
            throw new InvalidArgumentsException("nonTerminal niepoprawny");
        
        if(nonTerminal.length() != prod.length)
            throw new InvalidArgumentsException("Zła ilość produkcji");
        
        for(int i = 0; i<prod.length; ++i)
            if(prod[i] == null)
                throw new InvalidArgumentsException("Pusty zbiór symboli");
        
        // Skopiowanie tablicy prod i dodatkowe sprawdzenie poprawności
        String[][] prodCopy = new String[prod.length][];
        
        for(int i = 0; i<prod.length; ++i) {
            prodCopy[i] = new String[prod[i].length];
            
            for(int j = 0; j<prod[i].length; ++j) {
                prodCopy[i][j] = prod[i][j];
                
                // Wyjątek jeśli jakiś znak się nie zgadza
                for(int k = 0; k<prod[i][j].length(); ++k) {
                    CharSequence s = String.valueOf(prod[i][j].charAt(k));
                    
                    if(!terminal.contains(s) && !nonTerminal.contains(s))
                        throw new InvalidArgumentsException("Zły symbol w produkcji");
                }
            }
        }
        
        // Utworzenie gramatyki
        this.terminale = terminal;
        this.nonTerminale = nonTerminal;
        this.produkcje = prodCopy;
    }
    
    /**
     * Konstruktor kopiujący
     * @param grammar Referencja do gramatyki która ma być skopiowana 
     * @throws gramatyka.InvalidArgumentsException Jeżeli wystąpi ten wyjątek 
     * to jest bardzo źle i należy się schować przed promieniowaniem kosmicznym
     */
    protected ContextFreeGrammar(ContextFreeGrammar grammar) {
        this(grammar.terminale, grammar.nonTerminale, grammar.produkcje);
    }
    
    private static boolean isTerminalCorrect(String terminal)
    {
        // Czy właściwe znaki
        for(int i = 0; i<terminal.length(); ++i)
            if(!('a' <= terminal.charAt(i) && terminal.charAt(i) <= 'z'))
                return false;
        
        // Czy terminale się nie powtarzają
        for(int i = 0; i<terminal.length(); ++i)
            if(i != terminal.indexOf(terminal.charAt(i)))
                return false;
        
        return true;
    }
    
    private static boolean isNonterminalCorrect(String nonTerminal)
    {
        // Czy właściwe znaki
        for(int i = 0; i<nonTerminal.length(); ++i)
            if(!('A' <= nonTerminal.charAt(i) && nonTerminal.charAt(i) <= 'Z'))
                return false;
        
        // Czy nieterminale się nie powtarzają
        for(int i = 0; i<nonTerminal.length(); ++i)
            if(i != nonTerminal.indexOf(nonTerminal.charAt(i)))
                return false;
        
        return true;
    }
    
    /**
     * Sprawdza czy gramatyka jest regularna. Dla potrzeb niniejszego zadania 
     * przyjmujemy, że gramatyka regularna, to gramatyka w której wszystkie 
     * reguły są postaci: {@literal
     * A -> <puste> 
     * B -> b
     * C -> cD
     * 
     * albo postaci:
     * A -> <puste>
     * B -> b
     * C -> Dc
     * 
     * gdzie A , B, C, D oznacza nieterminale, b, c - terminale.
     * Nie jest dopuszczalne pojawienie się w tej samej gramatyce reguł postaci 
     * A -> bC i postaci C -> Dc. }
     * @return true jeśli tak, false w przeciwnym przypadku
     */
    public boolean ifRegular() {
        int typ = 0;
        // 0 - nieokreślony
        // 1 - pierwszy
        // 2 - drugi
        
        for(String[] p: produkcje)
            for(String s: p) {
                // Pusta produkcja
                if(s.equals(""))
                    continue;
                
                // Produkcja ze znakiem terminalnym
                if(s.length() == 1) {
                    if(terminale.contains(s))
                        continue;
                    else 
                        return false;
                }
                
                // Produkcj ze znakiem terminalnym i nieterminalnym
                if(s.length() > 2)
                    return false;
                
                // Ustalenie typu
                char fst = s.charAt(0);
                char snd = s.charAt(1);
                
                if(typ == 0) {
                    if(terminale.contains(String.valueOf(fst)) && 
                            nonTerminale.contains(String.valueOf(snd)))
                        typ = 1;
                    else if(terminale.contains(String.valueOf(snd)) && 
                            nonTerminale.contains(String.valueOf(fst)))
                        typ = 2;
                    else
                        return false;
                } else if(typ == 1) {
                    if(!terminale.contains(String.valueOf(fst)) || 
                            !nonTerminale.contains(String.valueOf(snd)))
                        return false;
                } else {
                    if(!terminale.contains(String.valueOf(snd)) || 
                            !nonTerminale.contains(String.valueOf(fst)))
                        return false;
                }    
            }

        return true;
    }
    
    /**
     * Sprawdza czy gramatyka jest w postaci Chomskiego. Postać normalna 
     * Chomskiego to postać gramatyki bezkontekstowej, w której wszystkie 
     * produkcje są tylko i wyłącznie w postaci: <br>
     * {@literal A -> a }   <br>
     * {@literal A -> BC }  <br>
     * gdzie małe litery oznaczają symbole terminalne, duże zaś nieterminalne.
     * @return true jeśli tak, false w przeciwnym przypadku
     */
    public boolean ifChomsky() {
        for(int i=0; i<produkcje.length; ++i)
            for(int j=0; j<produkcje[i].length; ++j) {
                if(2 < produkcje[i][j].length() || produkcje[i][j].length() < 1)
                    return false;
                
                // Produkcja w pierwszej postaci
                if(produkcje[i][j].length() == 1) {
                    CharSequence cs = String.valueOf(produkcje[i][j].charAt(0));
                    
                    if(!terminale.contains(cs))
                        return false;
                }
                
                // Produkcja w drugiej postaci
                if(produkcje[i][j].length() == 2) {
                    char f = produkcje[i][j].charAt(0);
                    char s = produkcje[i][j].charAt(1);
                    char l = nonTerminale.charAt(i);
                    
                    if(!nonTerminale.contains(String.valueOf(f)))
                        return false;
                    
                    if(!nonTerminale.contains(String.valueOf(s)))
                        return false;
                    
                    // "Rzeczywiście, w jednym miejscu na forum pisze, że 
                    // mają być róźne, więc niech tak będzie"
                    if(f == s || s == l || f == l)
                        return false;
                }
            }
        
        return true;
    }
    
    /**
     * Sprawdza czy wybrana produkcja jest w postaci Greibach
     * @param line numer symbolu nieterminalnego który zaczyna produkcję
     * @param num numer produkcji 
     * @return true jeśli produkcja jest w postaci Greibach
     */
    protected boolean isProductionInGreibachForm(int line, int num) {
        // Uniknięcie outOfRangeException
        if(produkcje[line][num].equals(""))
            return false;
                
        CharSequence z = String.valueOf(produkcje[line][num].charAt(0));
        if(!terminale.contains(z))
            return false;
                
        for(int k=1; k<produkcje[line][num].length(); ++k) {
            z = String.valueOf(produkcje[line][num].charAt(k));
            if(!nonTerminale.contains(z))
                return false;
        }
        
        return true;
    }
    
    /**
     * Sprawdza czy gramatyka jest w postaci Greibach. Tj. czy jej produkcje są
     * postaci {@literal A -> aX } gdzie a to dowolny symbol terminalny, X zaś 
     * to (być może pusty) ciąg symboli nieterminalnych.
     * @return true jeśli tak, false w przeciwnym przypadku
     */
    public boolean ifGreibach() {        
        for(int i=0; i<produkcje.length; ++i)
            for(int j=0; j<produkcje[i].length; ++j) 
                if(!isProductionInGreibachForm(i,j))
                    return false;

        return true;
    }
    
    /**
     * Zwraca napis zawierający typ gramatyki
     * @return napis w formacie {@literal <typ>/<postać> gdzie <typ>} oznacza 
     * bezkontekstowa albo regularna natomiast {@literal <postać>} oznacza 
     * Greibach, Chomsky albo napis pusty.
     */
    protected String getGrammarType() {
        if(grammarForm != null)
            return grammarForm;
        
        if(ifRegular())
            grammarForm = "regularna";
        else 
            grammarForm = "bezkontekstowa";
        
        if(ifChomsky())
            grammarForm += "/Chomsky";
        
        if(ifGreibach())
            grammarForm += "/Greibach";
        
        return grammarForm;
    }
    
    /**
     * Wypisuje gramatykę
     * @return Napis w postaci: <br> <i>
     * Gramatyka: {@literal <typ/postać gramatyki>} <br>
     * Terminale: {@literal <string zawierajacy symbole terminalne>} <br>
     * Nieterminale: {@literal <string zaierające symbole nieterminalne>} <br>
     * Produkcje: <br>
     * {@literal L -> P} <br>
     * ... <br> </i>
     * gdzie L i P odpowienio lewa i prawa strona kolejnych produkcji.
     * {@literal <typ/postać gramatyki>} powinien być jednym z napisów: 
     * bezkontekstowa, Chomskiego, Greibach, regularna.
     * Ewentualny symbol pusty wypisuje jako znak {@literal &}.
     */
    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("Gramatyka: ");
        s.append(getGrammarType());
        s.append("\n");
        
        s.append("Terminale: ");
        s.append(this.terminale);
        s.append("\n");
        
        s.append("Nieterminale: ");
        s.append(this.nonTerminale);
        s.append("\n");
        
        s.append("Produkcje:\n");
        
        for(int i=0; i<produkcje.length; ++i)
            for(int j=0; j<produkcje[i].length; ++j) {
                s.append(nonTerminale.charAt(i));
                s.append(" -> ");
                
                if(produkcje[i][j].equals(""))
                    s.append("&");
                else
                    s.append(produkcje[i][j]);
                
                s.append("\n");
            }

        return s.toString();
    }
}
