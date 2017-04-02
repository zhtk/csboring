package gramatyka;

/**
 * Klasa reprezentująca gramatykę regularną
 * @author Piotr
 */
public class RegularGrammar extends ContextFreeGrammar {
    /**
     * Konstruktor gramatyki regularnej
     * @param grammar gramatyka bezkontekstowa z której ma być utworzony obiekt. 
     * Przy wywoływaniu <b>należy</b> sprawdzić przy pomocy 
     * funkcji {@link ContextFreeGrammar#ifRegular()} czy to jest możliwe
     */
    public RegularGrammar(ContextFreeGrammar grammar) {
        super(grammar);
        
        if(!ifRegular())
            throw new InvalidArgumentsException("Gramatyka nie jest regularna");
    }
}
