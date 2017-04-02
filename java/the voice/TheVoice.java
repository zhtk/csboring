import java.io.File;
import java.util.ArrayList;

public class TheVoice {
    public static void execute(ArrayList<String> artists,
        ArrayList<Processor> processors,
        Filter filter,
        Source dataSource)
    {
        for(Processor p: processors) {
            for(String artist: artists) {
                String[][] texts = dataSource.getArtistTexts(artist);
                FilterableArtistData data = new FilterableArtistData(texts[1]);
                
                data.filter(filter);
                
                p.process(artist, data);
            }
            
            System.out.println(p);
            System.out.println("***");
        }
    }
    
    public static Source buildSourceType(String type) {
        if(type.compareTo("file") == 0)
            return new FileSource();
        
        System.out.println("Nieznane źródło tekstów");
        return null;
    }
    
    public static Processor buildProcessor(String type) {
        if(type.compareTo("count") == 0)
            return new CountingProcessor();
        else if(type.compareTo("top5") == 0)
            return new Top5Processor();
        
        System.out.println("Nieprawidłowy procesor!");
        return null;
    }
    
    public static void main(String[] args) {
        ArrayList<String> artists = new ArrayList<>();
        ArrayList<String> sources = new ArrayList<>();
        ArrayList<Processor> processors = new ArrayList<>();
        WordFilter filter = new WordFilter();
        Source dataSource = null;
        
        // Parsowanie argumentów
        for(String i: args) {
            if(i.contains("--source-type=")) {
                if(i.split("=").length != 2) {
                    System.out.println("Nieprawidłowy argument - typ źródła");
                    return;
                }
                
                dataSource = buildSourceType(i.split("=")[1]);
            } else if(i.contains("--source=")) {
                if(i.split("=").length != 2) {
                    System.out.println("Nieprawidłowy argument - źródło tekstów");
                    return;
                }
                
                sources.add(i.split("=")[1]);
            } else if(i.contains("--processors=")) {
                if(i.split("=").length != 2) {
                    System.out.println("Nieprawidłowy argument - procesory tekstów");
                    return;
                }
                
                String[] proc = i.split("=")[1].split(",");
                for(String s: proc) {
                    Processor p = buildProcessor(s);
                    if(p != null)
                        processors.add(p);
                }
            } else if(i.contains("--filters=")) {
                if(i.split("=").length != 2) {
                    System.out.println("Nieprawidłowy argument - filtry tekstów");
                    return;
                }
                
                String[] proc = i.split("=")[1].split(File.pathSeparator);
                for(String s: proc)
                   filter.loadWords(s);
            } else {
                artists.add(i);
            }
        }
        
        if(dataSource == null || sources.isEmpty()) {
            System.out.println("Nie podano źródła danych!");
            return;
        }
        
        for(String i: sources)
            dataSource.addSource(i);
        
        // Wykonanie poleceń
        execute(artists, processors, filter, dataSource);
    }
}
