// Piotr Zalas 361374

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void start(int szer, int wys, int *T);
void run();

int main(int argc, char* argv[]) {
	if (argc != 3) {
		printf("Usage: ./program input_file steps\n");
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
		printf("Height & width must be != 0!\n");
		return 1;
	}

	int *T = malloc(szer * wys * sizeof(int) * 2);

	if (!T) {
		printf("Malloc error\n");
		return 1;
	}

	for (int i = 0; i < wys; ++i)
		for (int j = 0; j < szer; ++j)
			fscanf(f, "%d", T + i * szer + j);

	fclose(f);

	memcpy(T + szer * wys, T, szer * wys * sizeof(int));

	int steps = 0;
	sscanf(argv[2], "%d", &steps);

	start(szer, wys, T);
	
	while (steps--) {
		run();

		for (int i = 0; i < wys; ++i) {
			for (int j = 0; j < szer; ++j)
				printf("%c", T[i * szer + j] ? '*' : ' ');
			printf("\n");
		}

		if (steps)
			getchar();
	}

	return 0;
}

