/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

/*
 * Hashtable with literal values for keys.
 */
#ifndef LT_H_
#define LT_H_

#include "ht.h"

#ifndef LT_C_

extern void* lt_get(struct ht_t *v, size_t key);
extern void lt_put(struct ht_t *v, size_t key, void *value);

#endif /* LT_C_ */
#endif /* LT_H_ */
