import java.util.ArrayList;

public class FilterableArtistData implements ArtistData 
{
    private String[][] songs;
    
    public FilterableArtistData(String[] songs)
    {
        if(songs == null) {
            this.songs = null;
            return;
        }
        
        this.songs = new String[songs.length][];
        for(int i=0; i<songs.length; ++i)
            this.songs[i] = songs[i].split("[\\s,.:;?!()-]+");
    }
    
    public void filter(Filter f)
    {
        if(songs == null)
            return;
        
        for(int i = 0; i < songs.length; ++i) {
            ArrayList<String> tmp = new ArrayList<>();
            
            for(String word: songs[i])
                if(!f.isBlacklisted(word)) 
                    tmp.add(word);
            
            songs[i] = tmp.toArray(new String[0]);
        }
    }
    
    @Override
    public String[] getSong(int num)
    {
        if(songs == null || num >= songs.length)
            return null;
        
        return songs[num];
    }
}
