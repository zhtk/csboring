import java.util.ArrayList;
import java.util.HashSet;

public class CountingProcessor implements Processor 
{
    private ArrayList<Result> results;
    
    protected class Result
    {
        public final String artist;
        public final int count;
        
        Result(String artist, int count)
        {
            this.artist = artist;
            this.count = count;
        }
    }
    
    public CountingProcessor()
    {
        results = new ArrayList<>();
    }
    
    @Override
    public void process(String artistName, ArtistData data)
    {
        HashSet<String> set = new HashSet<>();
        
        int i = 0;
        String[] song;
        while((song = data.getSong(i++)) != null) 
            for(String word: song)
                if(word.compareTo("") != 0)
                    set.add(word.toLowerCase());
        
        results.add(new Result(artistName, set.size()));
    }
    
    @Override
    public String toString()
    {
        results.sort( (Result r1, Result r2) -> {
            if(r1.count == r2.count)
                return 0;
            else if(r1.count > r2.count)
                return -1;
            else
                return 1;
        });
        
        StringBuilder sb = new StringBuilder();
        sb.append("count:\n");
        for(Result r: results)
            sb.append(r.artist).append(" ").append(r.count).append("\n");
        
        String res = sb.toString();
        return res.substring(0, res.length() - 1);
    }
}
