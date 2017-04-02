package gramatyka;

public class Main {
    public static void main(String[] args) {
        String[][] tab = {{"P","Q","R"},{"a","aP","aPb"},{"b", "Qb", "aQb"},{""}};
        
        try {
            // Zwykła gramatyka
            ContextFreeGrammar g = new ContextFreeGrammar("ab", "SPQR", tab);
            System.out.println(g);
            System.out.println("ifRegular() -> " + g.ifRegular());
            System.out.println("ifChomsky() -> " + g.ifChomsky());
            System.out.println("ifGreibach() -> " + g.ifGreibach());
            
            // Konwersja z postaci Chomskiego do Greibach
            // Przykład z wikipedii
            System.out.println( "=====================" );
            
            String[][] tab2 = 
                {{"BC"},{"CA"},{"BA", "a"}};
            g = new ContextFreeGrammar("a", "ABC", tab2);
            
            NormalChomskyGrammar cg = new NormalChomskyGrammar(g);
            System.out.println( cg );
            System.out.println( "=====================" );
            System.out.println( cg.toGreibach() );
            
            // Złośliwy przykład co miał zapętlać
            System.out.println( "=====================" );
            
            tab2 = new String[][]{{"BC", "a"},{"AC", "b"},{"BA", "c"}};
            g = new ContextFreeGrammar("abc", "ABC", tab2);
            
            cg = new NormalChomskyGrammar(g);
            System.out.println( cg );
            System.out.println( "=====================" );
            System.out.println( cg.toGreibach() );
        } catch(InvalidArgumentsException | ConversionException e) {
            System.out.println(e);
        }
    }
}
