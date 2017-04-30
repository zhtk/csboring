import java.sql.*;

public class MysqlTest {
    private static final int krok = 1000;
    private static final int obroty = 1000;
    
    public static void dodaj() throws SQLException
    {
        Connection connection = DriverManager.getConnection("jdbc:mysql://localhost:3306/test?user=root");
        Statement statement = connection.createStatement();
        statement.execute("CREATE TABLE IF NOT EXISTS test (id int) ENGINE = MEMORY;");
        
        for (int i = 0; i < krok; ++i)
            statement.execute("INSERT INTO test VALUES (42);");
        
        connection.close();
    }
    
    public static void test(int iteracja) throws SQLException
    {
        long start = System.nanoTime();
        
        Connection connection = DriverManager.getConnection("jdbc:mysql://localhost:3306/test?user=root");
        long opened = System.nanoTime();
        
        // Wykonanie zapytania
        Statement statement = connection.createStatement();
        statement.executeQuery("SELECT * FROM test;");
        long done = System.nanoTime();
        
        // Zamykanie połączenia
        connection.close();
        long closed = System.nanoTime();
        
        // Format danych:
        // Iteracja; czas otwarcia; czas zapytania; czas zamykania
        System.out.format("%d;%d;%d;%d;\n", iteracja, opened - start, 
                done - opened, closed - done);
    }
    
    public static void main(String[] args) {
        for (int i = 0; i < obroty; ++i) {
            try {
                dodaj();
                test(i);
            } catch(SQLException e) {
                System.out.println("Error" + e);
            }
        }
    }
    
}
