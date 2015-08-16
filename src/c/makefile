# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.

CC=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang
BASE_FLAGS=-Wall -Wextra -Wshadow -pedantic -I include
FLAGS=$(BASE_FLAGS) -O3
DEBUG_FLAGS=$(BASE_FLAGS) -O0 -g

ARCH_FLAGS_ARMV7=-arch armv7 -std=gnu99 -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS8.4.sdk
ARCH_FLAGS_ARMV7S=-arch armv7s -std=gnu99 -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS8.4.sdk
ARCH_FLAGS_ARM64=-arch arm64 -std=gnu99 -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS8.4.sdk
ARCH_FLAGS_X86_64=-arch x86_64 -std=gnu99
ARCH_FLAGS_I386=-arch i386 -std=gnu99
FLAGS_ARMV7=$(BASE_FLAGS) -O3 $(ARCH_FLAGS_ARMV7)
FLAGS_ARMV7S=$(BASE_FLAGS) -O3 $(ARCH_FLAGS_ARMV7S)
FLAGS_ARM64=$(BASE_FLAGS) -O3 $(ARCH_FLAGS_ARM64)
FLAGS_X86_64=$(BASE_FLAGS) -O3 $(ARCH_FLAGS_X86_64)
FLAGS_I386=$(BASE_FLAGS) -O3 $(ARCH_FLAGS_I386)

LIB_NAME_ARMV7=libwaxeye_armv7.a
LIB_NAME_ARM64=libwaxeye_arm64.a
LIB_NAME_ARMV7S=libwaxeye_armv7s.a
LIB_NAME_X86_64=libwaxeye_x86_64.a
LIB_NAME_I386=libwaxeye_i386.a

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


lib: clean-all libarm64 libarmv7 libarmv7s libi386 libx86_64
	lipo -create $(LIB_NAME_ARM64) $(LIB_NAME_ARMV7) $(LIB_NAME_ARMV7S) $(LIB_NAME_I386) $(LIB_NAME_X86_64) -o $(LIB_NAME)
	rm $(LIB_NAME_ARM64) $(LIB_NAME_ARMV7) $(LIB_NAME_ARMVS) $(LIB_NAME_I386) $(LIB_NAME_X86_64)
	mkdir -p $(LIB_DIR)
	cp $(LIB_NAME) $(LIB_DIR)

libarm64:
	$(CC) $(FLAGS_ARM64) -c -o ast.o ast.c
	$(CC) $(FLAGS_ARM64) -c -o cache.o cache.c
	$(CC) $(FLAGS_ARM64) -c -o edge.o edge.c
	$(CC) $(FLAGS_ARM64) -c -o fa.o fa.c
	$(CC) $(FLAGS_ARM64) -c -o ht.o ht.c
	$(CC) $(FLAGS_ARM64) -c -o input.o input.c
	$(CC) $(FLAGS_ARM64) -c -o lt.o lt.c
	$(CC) $(FLAGS_ARM64) -c -o set.o set.c
	$(CC) $(FLAGS_ARM64) -c -o state.o state.c
	$(CC) $(FLAGS_ARM64) -c -o trans.o trans.c
	$(CC) $(FLAGS_ARM64) -c -o vector.o vector.c
	$(CC) $(FLAGS_ARM64) -c -o wparser.o wparser.c
	ar cr $(LIB_NAME_ARM64) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)

libarmv7:
	$(CC) $(FLAGS_ARMV7) -c -o ast.o ast.c
	$(CC) $(FLAGS_ARMV7) -c -o cache.o cache.c
	$(CC) $(FLAGS_ARMV7) -c -o edge.o edge.c
	$(CC) $(FLAGS_ARMV7) -c -o fa.o fa.c
	$(CC) $(FLAGS_ARMV7) -c -o ht.o ht.c
	$(CC) $(FLAGS_ARMV7) -c -o input.o input.c
	$(CC) $(FLAGS_ARMV7) -c -o lt.o lt.c
	$(CC) $(FLAGS_ARMV7) -c -o set.o set.c
	$(CC) $(FLAGS_ARMV7) -c -o state.o state.c
	$(CC) $(FLAGS_ARMV7) -c -o trans.o trans.c
	$(CC) $(FLAGS_ARMV7) -c -o vector.o vector.c
	$(CC) $(FLAGS_ARMV7) -c -o wparser.o wparser.c
	ar cr $(LIB_NAME_ARMV7) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)

libarmv7s:
	$(CC) $(FLAGS_ARMV7S) -c -o ast.o ast.c
	$(CC) $(FLAGS_ARMV7S) -c -o cache.o cache.c
	$(CC) $(FLAGS_ARMV7S) -c -o edge.o edge.c
	$(CC) $(FLAGS_ARMV7S) -c -o fa.o fa.c
	$(CC) $(FLAGS_ARMV7S) -c -o ht.o ht.c
	$(CC) $(FLAGS_ARMV7S) -c -o input.o input.c
	$(CC) $(FLAGS_ARMV7S) -c -o lt.o lt.c
	$(CC) $(FLAGS_ARMV7S) -c -o set.o set.c
	$(CC) $(FLAGS_ARMV7S) -c -o state.o state.c
	$(CC) $(FLAGS_ARMV7S) -c -o trans.o trans.c
	$(CC) $(FLAGS_ARMV7S) -c -o vector.o vector.c
	$(CC) $(FLAGS_ARMV7S) -c -o wparser.o wparser.c
	ar cr $(LIB_NAME_ARMV7S) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)

libi386:
	$(CC) $(FLAGS_I386) -c -o ast.o ast.c
	$(CC) $(FLAGS_I386) -c -o cache.o cache.c
	$(CC) $(FLAGS_I386) -c -o edge.o edge.c
	$(CC) $(FLAGS_I386) -c -o fa.o fa.c
	$(CC) $(FLAGS_I386) -c -o ht.o ht.c
	$(CC) $(FLAGS_I386) -c -o input.o input.c
	$(CC) $(FLAGS_I386) -c -o lt.o lt.c
	$(CC) $(FLAGS_I386) -c -o set.o set.c
	$(CC) $(FLAGS_I386) -c -o state.o state.c
	$(CC) $(FLAGS_I386) -c -o trans.o trans.c
	$(CC) $(FLAGS_I386) -c -o vector.o vector.c
	$(CC) $(FLAGS_I386) -c -o wparser.o wparser.c
	ar cr $(LIB_NAME_I386) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)

libx86_64:
	$(CC) $(FLAGS_X86_64) -c -o ast.o ast.c
	$(CC) $(FLAGS_X86_64) -c -o cache.o cache.c
	$(CC) $(FLAGS_X86_64) -c -o edge.o edge.c
	$(CC) $(FLAGS_X86_64) -c -o fa.o fa.c
	$(CC) $(FLAGS_X86_64) -c -o ht.o ht.c
	$(CC) $(FLAGS_X86_64) -c -o input.o input.c
	$(CC) $(FLAGS_X86_64) -c -o lt.o lt.c
	$(CC) $(FLAGS_X86_64) -c -o set.o set.c
	$(CC) $(FLAGS_X86_64) -c -o state.o state.c
	$(CC) $(FLAGS_X86_64) -c -o trans.o trans.c
	$(CC) $(FLAGS_X86_64) -c -o vector.o vector.c
	$(CC) $(FLAGS_X86_64) -c -o wparser.o wparser.c
	ar cr $(LIB_NAME_X86_64) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)


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
