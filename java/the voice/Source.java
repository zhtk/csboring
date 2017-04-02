public interface Source {
    public void addSource(String src);
    /**
     * Zwraca dwuwymiarową tablicę zawierającą nazwy piosenek artysty i ich 
     * teksty.
     * @param artist Nazwa artysty.
     * @return Tablica, której pierwszy wymiar ma 2 elementy:
     * pierwszy to tablica z nazwami piosenek a drugi to tablica z tekstami
     * piosenek. Elementy w tych dwóch tablicach są ze sobą skorelowane w taki 
     * sposób, że i-ta nazwa piosenki (ze współrzędnej [0][i]) odpowiada i-temu
     * tekstowi (o współrzędnej [1][i]).
     */
    public String[][] getArtistTexts(String artist);
}
