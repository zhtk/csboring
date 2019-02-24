#include <stdio.h>
#include <string.h>

#include "ppm.h"
#include "pgm.h"

extern uint8_t rscale;
extern uint8_t gscale;
extern uint8_t bscale;
gray ** convert(pixel **image, int cols, int rows);

int main(int argc, char* argv[]) {
	if (argc != 2) {
		printf("Usage: ./program file.ppm\n");
		return 1;
	}

	int filenamelen = strlen(argv[1]);
	if (filenamelen < 3) {
		printf("File name too short (no extension): %s\n", argv[1]);
		return 1;
	}

	FILE *fp = fopen(argv[1], "r");

	if (fp == NULL) {
		printf("Unable to open file %s\n", argv[1]);
		return 1;
	}

	int cols, rows;
	pixval maxval;

	pixel **image = ppm_readppm(fp, &cols, &rows, &maxval);
	fclose(fp);

	gray **result = convert(image, cols, rows);

	argv[1][filenamelen - 2] = 'g';

	fp = fopen(argv[1], "w");
	pgm_writepgm(fp, result, cols, rows, 256, 1);
	fclose(fp);

	return 0;
}
