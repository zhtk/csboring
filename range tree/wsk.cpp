#include <cstdio>
#include <vector>
#include <set>
#include <algorithm>
#include <stack>
using namespace std;

// Drzewo przedziałowe i operacje na nim
struct vec {
	long long int x, y;
	int kierunek; // Zgodny z wskazówkami zegara

	vec() : x(0), y(0), kierunek(0) {}
};

const int tsize = 1<<21;
vector<vec> tree(tsize);

void obrot(vec& v)
{
	v.kierunek++;
	v.kierunek %= 4;

	swap(v.x, v.y);
	v.y *= -1;
}

vec merge(vec l, vec r)
{
	// Obrót wektora r
	for(int i = 0; i < l.kierunek; ++i)
		obrot(r);
	
	// Złączenie
	l.x += r.x;
	l.y += r.y;
	l.kierunek = r.kierunek;

	return l;
}

vec get(int to)
{
	to += tsize / 2;
	vec wynik = tree[to];
	
	while(to) {
		if(to != 1 && (to & 1))
			wynik = merge(tree[to - 1], wynik);

		to /= 2;
	}

	return wynik;
}

void put(int nr, vec v)
{
	nr += tsize / 2;

	tree[nr] = v;
	nr /= 2;

	while(nr) {
		tree[nr] = merge(tree[nr * 2], tree[nr * 2 + 1]);
		nr /= 2;
	}
}

// ====================================

// Drzewo przedziałowe

vector<int> rtr(tsize, 0);

void rinsert(int a)
{
	a += tsize / 2;
	
	while(a) {
		++rtr[a];
		a /= 2;
	}
}

int rget(int n)
{
	int wynik = 1;
	int rdl = tsize / 2;

	while(wynik < tsize / 2) {
		rdl /= 2;
		wynik *= 2;
	
		if(rdl - rtr[wynik] < n) {
			n -= rdl - rtr[wynik];
			++wynik;
		}
	}
	
	wynik -= tsize / 2;
	return wynik;
}

// ====================================

int main()
{
	int n;
	scanf("%d", &n);

	int krok[n];
        long long int odl[n];
	char oper[n];

	for(int i = 0; i < n; ++i)
		scanf("%d%*c%c%lld", krok + i, oper + i, odl + i);

	// Ustalenia porządku wskazówek
	for(int i = n-1; i >= 0; --i) {
		int tmp = rget(krok[i]);
		rinsert(tmp);
		krok[i] = tmp;
		// printf("i = %d, krok = %d\n", i+1, tmp);
	}
	
	// Wykonanie kroków
	for(int i = 0; i < n; ++i) {
		vec v;
		
		// Ustawienie kierunku i przesunięcia na podstawie kroku
		if(oper[i] == 'L') {
			v.x = -odl[i];
			v.kierunek = 3;
		} else if(oper[i] == 'R') {
			v.kierunek = 1;
			v.x = odl[i];
		} else if(oper[i] == 'U') {
			v.kierunek = 2;
			v.y = -odl[i];
		}

		// Wykonaie zapytania
		put(krok[i], v);
		v = get(krok[i]);

		printf("%lld %lld\n", v.x, v.y);
	}

	return 0;
}
