import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Top5Processor implements Processor 
{
    private ArrayList<String> results;
    
    public Top5Processor()
    {
        results = new ArrayList<>();
    }
    
    protected class WordStat 
    {
        public final String word;
        public final int count;
        
        WordStat(String w, int c) 
        {
            word = w;
            count = c;
        }
    }
    
    @Override
    public void process(String artistName, ArtistData data)
    {
        HashMap<String, Integer> map = new HashMap();
        
        // Zliczenie słów
        int i = 0;
        String[] song;
        while((song = data.getSong(i++)) != null) 
            for(String word: song) {
                word = word.toLowerCase();
                
                if(map.containsKey(word)) 
                    map.put(word, map.get(word) + 1);
                else 
                    map.put(word, 1);
            }
        
        // Przepisanie do sortowalnego pojemnika
        ArrayList<WordStat> stat = new ArrayList<>();
        
        for(Map.Entry pair: map.entrySet())
            stat.add(new WordStat((String) pair.getKey(), (Integer) pair.getValue()));
        
        // Posortowanie
        stat.sort((WordStat s1, WordStat s2) -> {
            if(s1.count == s2.count)
                return s1.word.compareTo(s2.word);
            else if(s1.count > s2.count)
                return -1;
            else
                return 1;
        });
        
        // Wybranie top5
        String res = "[";
        
        for(i = 0; i < 5 && i < stat.size(); ++i) {
            WordStat ws = stat.get(i);
            
            res += ws.word + "=" + ws.count+", ";
        }
        
        if(res.length() > 2)
            res = res.substring(0, res.length() - 2);
        res+="]";
        results.add(artistName + "\n" + res);
    }
    
    @Override
    public String toString()
    {        
        StringBuilder sb = new StringBuilder();
        sb.append("top5:\n");
        
        for(String i: results)
            sb.append(i).append("\n");
        
        String res = sb.toString();
        return res.substring(0, res.length() - 1);
    }
}
