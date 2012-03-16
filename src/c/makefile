# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.

CC=gcc
BASE_FLAGS=-Wall -Wextra -Wshadow -pedantic -I include
FLAGS=$(BASE_FLAGS) -O3
DEBUG_FLAGS=$(BASE_FLAGS) -O0 -g

LIB_NAME=libwaxeye.a
LIB_DIR=../../lib/
LIB=$(LIB_DIR)$(LIB_NAME)

LIB_FILES= \
	ast \
	cache \
	edge \
	fa \
	ht \
	input \
	lt \
	set \
	state \
	trans \
	vector \
	wparser

LIB_OBJECTS = $(foreach f, $(LIB_FILES), $(f).o)


lib: clean-all
	$(CC) $(FLAGS) -c -o ast.o ast.c
	$(CC) $(FLAGS) -c -o cache.o cache.c
	$(CC) $(FLAGS) -c -o edge.o edge.c
	$(CC) $(FLAGS) -c -o fa.o fa.c
	$(CC) $(FLAGS) -c -o ht.o ht.c
	$(CC) $(FLAGS) -c -o input.o input.c
	$(CC) $(FLAGS) -c -o lt.o lt.c
	$(CC) $(FLAGS) -c -o set.o set.c
	$(CC) $(FLAGS) -c -o state.o state.c
	$(CC) $(FLAGS) -c -o trans.o trans.c
	$(CC) $(FLAGS) -c -o vector.o vector.c
	$(CC) $(FLAGS) -c -o wparser.o wparser.c
	ar cr $(LIB_NAME) $(LIB_OBJECTS)
	mkdir -p $(LIB_DIR)
	cp $(LIB_NAME) $(LIB_DIR)


debug-lib: clean-all
	$(CC) $(DEBUG_FLAGS) -c -o ast.o ast.c
	$(CC) $(DEBUG_FLAGS) -c -o cache.o cache.c
	$(CC) $(DEBUG_FLAGS) -c -o edge.o edge.c
	$(CC) $(DEBUG_FLAGS) -c -o fa.o fa.c
	$(CC) $(DEBUG_FLAGS) -c -o ht.o ht.c
	$(CC) $(DEBUG_FLAGS) -c -o input.o input.c
	$(CC) $(DEBUG_FLAGS) -c -o lt.o lt.c
	$(CC) $(DEBUG_FLAGS) -c -o set.o set.c
	$(CC) $(DEBUG_FLAGS) -c -o state.o state.c
	$(CC) $(DEBUG_FLAGS) -c -o trans.o trans.c
	$(CC) $(DEBUG_FLAGS) -c -o vector.o vector.c
	$(CC) $(DEBUG_FLAGS) -c -o wparser.o wparser.c
	ar cr $(LIB_NAME) $(LIB_OBJECTS)
	mkdir -p $(LIB_DIR)
	cp $(LIB_NAME) $(LIB_DIR)


run: clean-all lib
	$(CC) $(FLAGS) parser.c run.c $(LIB) -o run


debug: clean-all debug-lib
	$(CC) $(DEBUG_FLAGS) parser.c run.c $(LIB) -o run


test: clean
	$(CC) $(FLAGS) cache.c test.c -o run


clean:
	rm -f run *.o *.a


clean-lib:
	rm -f $(LIB)


clean-all: clean clean-lib
