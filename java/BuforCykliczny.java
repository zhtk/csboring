public class BuforCykliczny {
    private int size;
    // Elementy są na przedziale [begin -> end)
    private int begin, end;
    private int[] tab;
    
    private void realloc(){
        if(tab==null)
            tab = new int[2];
        else {
            int[] ntab = new int[tab.length*2];
            
            int i=begin;
            for(int ptr=0; ptr<size; ++ptr, ++i)
                ntab[ptr]=tab[i%tab.length];
            
            begin = 0;
            end = size;
            tab = ntab;
        }
    }
    
    void pushFront(int x){
        if(tab == null || size == tab.length)
            realloc();
        
        --begin;
        if(begin<0)
            begin+=tab.length;
        
        tab[begin] = x;
        ++size;
    }
    
    int popFront(){
        int ret = tab[begin];
        
        --size;
        ++begin;
        if(begin>=tab.length)
            begin=0;
        
        return ret;
    }
    
    void pushBack(int x){
        if(tab == null || size == tab.length)
            realloc();
        
        tab[end] = x;
        end=(end+1)%tab.length;
        ++size;
    }
    
    int popBack(){      
       --end;
       --size;
       if(end<0)
           end+=tab.length;
       
       return tab[end];
    }
    
    int size(){
        return size;
    }
            
    boolean isEmpty(){
        return size()==0;
    }
}
