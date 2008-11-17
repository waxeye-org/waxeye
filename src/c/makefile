# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008 Orlando D. A. R. Hill
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is furnished to do
# so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

CC=gcc
BASE_FLAGS=-Wall -Wextra -Wshadow -I include
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


debug: clean-all debug_lib
	$(CC) $(DEBUG_FLAGS) parser.c run.c $(LIB) -o run


test: clean
	$(CC) $(FLAGS) cache.c test.c -o run


clean:
	rm -f run *.o *.a


clean-lib:
	rm -f $(LIB)


clean-all: clean clean-lib
