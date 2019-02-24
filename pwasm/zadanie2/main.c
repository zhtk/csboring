// Piotr Zalas 361374

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void start(int szer, int wys, float *M, float *G, float *C, float waga);
void step();

int main(int argc, char* argv[]) {
	if (argc != 4) {
		printf("Usage: ./symulator input_file steps weight\n");
		return 1;
	}

	FILE* f = fopen(argv[1], "r");

	if (f == NULL) {
		printf("Wrong input file name: %s!\n", argv[1]);
		return 1;
	}

	int szer, wys;
	fscanf(f, "%d %d", &szer, &wys);

	if (szer == 0 || wys == 0) {
		printf("Height & width must be =/= 0!\n");
		return 1;
	}

	// Dwie macierze
	float *M = aligned_alloc(0x10, (szer + 2) * (wys + 2) * 2 * sizeof(float));
	float *G = malloc(sizeof(float) * szer), *C = malloc(sizeof(float) * wys);

	if (M == NULL || G == NULL || C == NULL) {
		printf("Malloc error\n");
		return 1;
	}

	for (int i = 1; i <= wys; ++i)
		for (int j = 1; j <= szer; ++j)
			fscanf(f, "%f", M + i * (szer + 2) + j);

	for (int i = 0; i < szer; ++i)
		fscanf(f, "%f", G + i);

	for (int i = 0; i < wys; ++i)
		fscanf(f, "%f", C + i);

	fclose(f);

	for (int i = 0; i < szer; ++i) {
		M[i + 1] = G[i];
		M[i + 1 + (szer + 2) * (wys + 1)] = G[i];
	}

	for (int i = 1; i <= wys; ++i) {
		M[i * (szer + 2)] = C[i - 1];
		M[i * (szer + 2) + szer + 1] = C[i - 1];
	}

	// Skopiowanie tak uzyskanej matrycy
	memcpy(M + (szer + 2) * (wys + 2), M, (szer + 2) * (wys + 2) * sizeof(float));

	int steps = 0;
	float waga;
	sscanf(argv[2], "%d", &steps);
	sscanf(argv[3], "%f", &waga);

	start(szer, wys, M, G, C, waga);
	
	while (steps--) {
		step();

		for (int i = 0; i < wys + 2; ++i) {
			for (int j = 0; j < szer + 2; ++j)
				printf("%.3f\t", M[i * (szer + 2) + j]);
			printf("\n");
		}

		if (steps)
			getchar();
	}

	return 0;
}

