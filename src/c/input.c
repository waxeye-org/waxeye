/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008 Orlando D. A. R. Hill
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <waxeye/vector.h>

#define INPUT_C_
#include <waxeye/input.h>

#define FILLED_INIT 128
#define BLOCK_SIZE 1024

char* make_block() {
    char* block = malloc(BLOCK_SIZE * sizeof(char));
    assert(block != NULL);
    return block;
}

/*
 * Initializes the given input structure.
 */
void input_init(struct input_t *b, char *data, size_t size) {
    assert(data != NULL);
    b->data = data;
    b->size = size;
    b->pos = 0;
}


/*
 * Creates a new input buffer with the given data and size.
 */
struct input_t* input_new(char *data, size_t size) {
    struct input_t *b = malloc(sizeof(struct input_t));
    assert(b != NULL);

    input_init(b, data, size);

    return b;
}


/*
 * Creates a new input buffer filled with the contents of the given FILE.
 */
struct input_t* input_from_file_new(FILE *fp) {
    struct vector_t *filled = vector_new(FILLED_INIT);
    char *block = make_block();
    size_t size = 0;
    size_t count;

    count = fread(block, sizeof(char), BLOCK_SIZE, fp);
    size += count;

    while (count == BLOCK_SIZE) {
        vector_add(filled, block);
        block = make_block();
        count = fread(block, sizeof(char), BLOCK_SIZE, fp);
        size += count;
    }

    // Create a contiguous piece of memory for our data
    char *data = malloc(size * sizeof(char));
    char *tmp = data;

    size_t i, block_size = BLOCK_SIZE * sizeof(char);
    size_t num_filled = filled->size;

    for (i = 0; i < num_filled; i++) {
        char *fb = vector_get(filled, i);
        memcpy(tmp, fb, block_size);
        free(fb);
        tmp += block_size;
    }

    // Transfer the data from the partially filled block
    memcpy(tmp, block, count);

    // Free the memory used in filling our input buffer
    free(block);
    vector_delete(filled);

    return input_new(data, size);
}


void input_delete(struct input_t *b) {
    free(b);
}


void input_and_data_clear(struct input_t *b) {
    free(b->data);
    b->data = NULL;
}


void input_and_data_delete(struct input_t *b) {
    input_and_data_clear(b);
    free(b);
}


/*
 * Whether the end of the input has been reached.
 */
bool input_eof(struct input_t *b) {
    return b->pos >= b->size ? true : false;
}


/*
 * Consumes one char of input and returns it.
 * Be sure to test for input_eof before using.
 */
char input_consume(struct input_t *b) {
    return input_eof(b) ? 0 : b->data[b->pos++];
}


/*
 * Peeks at the next char of input.
 * Be sure to test for input_eof before using.
 */
char input_peek(struct input_t *b) {
    return input_eof(b) ? 0 : b->data[b->pos];
}
