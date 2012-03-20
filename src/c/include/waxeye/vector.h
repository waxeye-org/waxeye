/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef VECTOR_H_
#define VECTOR_H_

#ifdef __cplusplus
extern "C" {
#endif

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

extern void vector_reverse(struct vector_t *v);

#endif /* VECTOR_C_ */

#ifdef __cplusplus
}
#endif

#endif /* VECTOR_H_ */
