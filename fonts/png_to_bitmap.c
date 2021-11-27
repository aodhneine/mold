#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

typedef unsigned char u8;
typedef unsigned int u32;
typedef signed int i32;

struct image_info {
	i32 width, height, depth;
	u8 *data;
};

struct font_info {
	/* Width and height of each glyph in pixels. */
	u32 width, height;
};

int main(void) {
	struct image_info image;
	image.data = stbi_load("font.png", &image.width, &image.height, &image.depth, 0);

	if (image.data == NULL) {
		perror("stbi_load");
		return 1;
	}

	struct font_info font = {
		.width = 6,
		.height = 13,
	};

	/* 6 glyph lines * 13 lines per glyph * 12 bytes in each glyph line */
	u8 *bitmap = malloc(12 * font.height * 6);

	if (bitmap == NULL) {
		perror("malloc");
		return 1;
	}

	u8 *ptr = bitmap;
	u8 mask = 1 << 7;

	u32 *data = (u32 *) image.data;

	for (int i = 0; i < image.height; ++i) {
		for (int j = 0; j < image.width; ++j) {
			u32 colour = data[i * image.width + j];

			switch (colour) {
			/* White. */
			case 0xffffffff:
				*ptr |= mask;
				break;
			/* Black in ABGR. */
			case 0xff000000:
				*ptr &= ~mask;
				break;
			default:
				break;
			}

			if (mask == 1) {
				mask = 1 << 7;
				++ptr;
			} else {
				mask = mask >> 1;
			}
		}
	}

	int file = open("font.raw", O_CREAT | O_TRUNC | O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);

	if (file == -1) {
		perror("open");
		return 1;
	}

	ssize_t result = write(file, bitmap, 12 * font.height * 6);

	if (result == -1) {
		perror("write");
		return 1;
	}

	if (result < 12 * font.height * 6) {
		fprintf(stderr, "wrote less bytes than expected!");
	}

	stbi_image_free(image.data);
	return 0;
}
