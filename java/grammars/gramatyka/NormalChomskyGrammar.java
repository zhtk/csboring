package gramatyka;

/**
 * Klasa reprezentująca gramatykę Chomskiego
 * @author piotr
 */
public class NormalChomskyGrammar extends ContextFreeGrammar {
    /**
     * Konstruktor gramatyki
     * @param grammar gramatyka bezkontekstowa w postaci Chomskiego z której ma
     * być utworzony obiekt. Przy wywoływaniu <b>należy</b> sprawdzić przy pomocy 
     * funkcji {@link ContextFreeGrammar#ifChomsky()} czy to jest możliwe
     */
    public NormalChomskyGrammar(ContextFreeGrammar grammar) {
        super(grammar);
        
        if(!ifChomsky())
            throw new InvalidArgumentsException("Gramatyka nie jest w postaci Chomskiego");
    }
    
    /**
     * Funkcja konwertująca gramatykę do postaci Greibach
     * @return gramatyka w postaci Greibach
     * @throws gramatyka.ConversionException Wyjątek rzucany gdy konwersja się 
     * nie powiedzie.
     */
    public NormalGreibachGrammar toGreibach() throws ConversionException {
        try {
            return new NormalGreibachGrammar(this);
        } catch(ArrayIndexOutOfBoundsException | NullPointerException e) {
            throw new ConversionException();
        }
    }
}
