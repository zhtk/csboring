import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;

public class FileSource implements Source {
    private String path;
    
    @Override
    public void addSource(String src)
    {
        path = src;
    }
    
    @Override
    public String[][] getArtistTexts(String artist)
    {
        ArrayList<String> texts = new ArrayList<>();
        ArrayList<String> names = new ArrayList<>();
        File folder = new File(path+"/"+artist+"/");
        
        if(!folder.isDirectory()) {
            System.out.println("Brak artysty w bazie danych: "+artist);
            return null;
        }
        
        File[] filesArray = folder.listFiles();
        
        if(filesArray == null)
            return null;
        
        for(File f: filesArray) {
            byte bytes[];
            
            try {
                if(!f.toPath().toString().endsWith(".txt"))
                    continue;
                
                bytes = Files.readAllBytes(f.toPath());
            } catch (IOException ex) {
                System.out.println("Błąd przy wczytywaniu piosenki: "+f.toPath());
                System.out.println(ex);
                return null;
            }
            
            names.add(f.getName().substring(0, f.getName().length() - 4));
            String text = new String(bytes, StandardCharsets.UTF_8);
            texts.add(text);
        }
        
        String[][] ret = new String[2][];
        
        if(texts.isEmpty())
            ret[0] = ret[1] = null;
        else {
            ret[0] = names.toArray(new String[texts.size()]);
            ret[1] = texts.toArray(new String[texts.size()]);
        }
        
        return ret;
    }
}
