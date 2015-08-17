# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Copyright (C) 2015 Joshua Gross
# Licensed under the MIT license. See 'LICENSE' for details.

CLANG=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang
GCC=gcc

BASE_FLAGS=-Wall -Wextra -Wshadow -pedantic -I include
FLAGS=$(BASE_FLAGS) -O3
DEBUG_FLAGS=$(BASE_FLAGS) -O0 -g

IOS_SDK=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS8.4.sdk
ARCH_FLAGS_ARMV7=-arch armv7 -std=gnu99 -isysroot $(IOS_SDK)
ARCH_FLAGS_ARMV7S=-arch armv7s -std=gnu99 -isysroot $(IOS_SDK)
ARCH_FLAGS_ARM64=-arch arm64 -std=gnu99 -isysroot $(IOS_SDK)
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
LIB_NAME_IOS=libwaxeye_ios.a
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

lib: clean-all lib_default
	mkdir -p $(LIB_DIR)
	cp $(LIB_NAME) $(LIB_DIR)

# All architectures required for iOS dev except for ARMv6 which is too old to care about
lib_iOS: clean-all libarm64 libarmv7 libarmv7s libi386 libx86_64
	lipo -create $(LIB_NAME_ARM64) $(LIB_NAME_ARMV7) $(LIB_NAME_ARMV7S) $(LIB_NAME_I386) $(LIB_NAME_X86_64) -o $(LIB_NAME_IOS)
	rm $(LIB_NAME_ARM64) $(LIB_NAME_ARMV7) $(LIB_NAME_ARMVS) $(LIB_NAME_I386) $(LIB_NAME_X86_64)
	mkdir -p $(LIB_DIR)
	cp $(LIB_NAME_IOS) $(LIB_DIR)

# Build a lib just for the current arch
lib_default:
	$(foreach f, $(LIB_FILES), $(GCC) $(FLAGS) -c -o $(f).o $(f).c;)
	ar cr $(LIB_NAME) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)

libarm64:
	$(foreach f, $(LIB_FILES), $(CLANG) $(FLAGS_ARM64) -c -o $(f).o $(f).c;)
	ar cr $(LIB_NAME_ARM64) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)

libarmv7:
	$(foreach f, $(LIB_FILES), $(CLANG) $(FLAGS_ARMV7) -c -o $(f).o $(f).c;)
	ar cr $(LIB_NAME_ARMV7) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)

libarmv7s:
	$(foreach f, $(LIB_FILES), $(CLANG) $(FLAGS_ARMV7S) -c -o $(f).o $(f).c;)
	ar cr $(LIB_NAME_ARMV7S) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)

libi386:
	$(foreach f, $(LIB_FILES), $(CLANG) $(FLAGS_I386) -c -o $(f).o $(f).c;)
	ar cr $(LIB_NAME_I386) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)

libx86_64:
	$(foreach f, $(LIB_FILES), $(CLANG) $(FLAGS_X86_64) -c -o $(f).o $(f).c;)
	ar cr $(LIB_NAME_X86_64) $(LIB_OBJECTS)
	rm $(LIB_OBJECTS)


debug-lib: clean-all
	$(foreach f, $(LIB_FILES), $(GCC) $(DEBUG_FLAGS) -c -o $(f).o $(f).c;)
	ar cr $(LIB_NAME) $(LIB_OBJECTS)
	mkdir -p $(LIB_DIR)
	cp $(LIB_NAME) $(LIB_DIR)


run: clean-all lib
	$(GCC) $(FLAGS) parser.c run.c $(LIB) -o run

ios: clean-all lib_iOS


debug: clean-all debug-lib
	$(GCC) $(DEBUG_FLAGS) parser.c run.c $(LIB) -o run


test: clean
	$(GCC) $(FLAGS) cache.c test.c -o run


clean:
	rm -f run *.o *.a


clean-lib:
	rm -f $(LIB)


clean-all: clean clean-lib
