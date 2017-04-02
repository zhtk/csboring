package gramatyka;

/**
 * Klasa reprezentująca wyjątek rzucany gdy do konstruktora zostaną przekazane
 * bezsensowne argumenty
 */
public class InvalidArgumentsException extends RuntimeException {
    /**
     * Konstruktor wyjątku
     * @param msg Wiadomość co poszło nie tak
     */
    public InvalidArgumentsException(String msg) {
        super(msg);
    }
}
