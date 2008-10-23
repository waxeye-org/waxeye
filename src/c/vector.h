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

#ifndef VECTOR_H_
#define VECTOR_H_

struct vector_t {
    void **elements;
    size_t capacity;
    size_t size;
};


#ifndef VECTOR_C_

extern void vector_init(struct vector_t *v, size_t capacity);
extern struct vector_t* vector_new(size_t capacity);
extern void vector_clear(struct vector_t *v);
extern void vector_delete(struct vector_t *v);
extern void* vector_get(struct vector_t *v, size_t index);
extern size_t vector_add(struct vector_t *v, void *element);

extern void vector_push(struct vector_t *v, void *element);
extern void* vector_pop(struct vector_t *v);
extern void* vector_peek(struct vector_t *v);

#endif /* VECTOR_C_ */
#endif /* VECTOR_H_ */
