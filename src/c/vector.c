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

#define VECTOR_C_
#include "vector.h"

/*
 * Initializes the given vector.
 * Each element of the vector is initially NULL.
 * v - The vector to init.
 * capacity - The initial capacity.
 */
void vector_init(struct vector_t *v, size_t capacity) {
    v->elements = calloc(capacity, sizeof(void*));
    assert(v->elements != NULL);
    v->capacity = capacity;
    v->size = 0;
}

/*
 * Creates a new vector with the given capacity.
 */
struct vector_t* vector_new(size_t capacity) {
    struct vector_t *v = malloc(sizeof(struct vector_t));
    assert(v != NULL);
    vector_init(v, capacity);
    return v;
}

/*
 * Frees the element storage of the vector.
 * Doesn't free the data of individual elements or the vector itself.
 * v - The vector to free.
 */
void vector_clear(struct vector_t *v) {
    free(v->elements);
    v->elements = NULL;
}

/*
 * Frees the vector deallocates the memory at the given pointer.
 * v - The vector to delete.
 */
void vector_delete(struct vector_t *v) {
    vector_clear(v);
    free(v);
}

/*
 * Gets the element at the given index from the given vector.
 * v - The vector to access.
 * index - The index of the element to get.
 */
void* vector_get(struct vector_t *v, size_t index) {
    assert(index < v->size);
    return v->elements[index];
}

/*
 * Adds the element to the vector and returns the index.
 * v - The vector to access.
 * element - The element to add.
 */
size_t vector_add(struct vector_t *v, void *element) {
    if (v->size == v->capacity) {

        v->capacity = v->capacity * 2;
        assert(v->capacity > v->size);

        v->elements = realloc(v->elements, v->capacity * sizeof(void*));
        assert(v->elements != NULL);
    }

    v->elements[v->size] = element;

    return v->size++;
}


void vector_push(struct vector_t *v, void *element) {
    vector_add(v, element);
}


void* vector_pop(struct vector_t *v) {
    void *e = vector_get(v, v->size - 1);

    v->elements[v->size - 1] = NULL;
    v->size = v->size - 1;

    return e;
}


void* vector_peek(struct vector_t *v) {
    return vector_get(v, v->size - 1);
}


void vector_reverse(struct vector_t *v) {
    void *tmp;
    size_t i, last = v->size - 1, half = v->size / 2;

    for (i = 0; i < half; i++) {
        tmp = v->elements[i];
        v->elements[i] = v->elements[last - i];
        v->elements[last - i] = tmp;
    }
}
