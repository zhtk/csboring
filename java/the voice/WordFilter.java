import java.io.*;
import java.util.HashSet;

public class WordFilter implements Filter 
{
    private HashSet<String> set;
    
    public WordFilter()
    {
        set = new HashSet<>();
    }
    
    public void loadWords(String path) 
    {
        try {
            File fileDir = new File(path);
            BufferedReader in = new BufferedReader(
                new InputStreamReader(new FileInputStream(fileDir), "UTF8")
            );
            
            String str;
            while ((str = in.readLine()) != null) 
                set.add(str.toLowerCase());
            
            in.close();
        } 
        catch (UnsupportedEncodingException e) {
            System.out.println("Nie można wczytać filtru");
            System.out.println(e.getMessage());
        } catch (IOException e) {
            System.out.println("Nie można wczytać filtru");
            System.out.println(e.getMessage());
        }
    }
    
    @Override
    public boolean isBlacklisted(String word)
    {
        return set.contains(word.toLowerCase());
    }
}
