public interface Processor 
{
    public void process(String artistName, ArtistData data);
    
    @Override
    public abstract String toString();
}
