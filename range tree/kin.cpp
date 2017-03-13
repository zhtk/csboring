#include <cstdio>
#include <vector>

using namespace std;

const int mod = 1e9;
const int vsize = 1<<16;

void dodaj(vector<int> &v, int liczba, int ile)
{
	int node = vsize/2 + liczba;

	while(node) {
		v[node] += ile;
		v[node] %= mod;
		node /= 2;
	}
}

int ilew(vector<int> &v, int liczba)
{
	int wynik = 0;
	int node = vsize/2 + liczba;
	
	// Ma być: ile większych!
	// wynik += v[node];

	while(node > 1) {
		if(node%2 == 0)
			wynik += v[node + 1];
		wynik %= mod;
		node /= 2;
	}

	return wynik;
}

int main()
{
	vector<int> v[11];//(vsize, 0);
	for(int i=0; i<11; ++i)
		v[i] = vector<int>(vsize, 0);

	int n, k;
	scanf("%d%d", &n, &k);

	// Obliczenia
	int wynik = 0;
	
	for(int i = 0; i < n; ++i) {
		int tmp;
		scanf("%d", &tmp);
		
		dodaj(v[1], tmp, 1);

		for(int j = 2; j <= k; ++j) {
			int iw = ilew(v[j - 1], tmp);
			
			if (j == k) {
				wynik += iw;
				wynik %= mod;
			}

			dodaj(v[j], tmp, iw);
		}
	}

	printf("%d\n", (k == 1 ? n : wynik));
	return 0;
}
