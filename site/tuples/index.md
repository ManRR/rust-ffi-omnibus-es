---
layout: default
examples: ../examples/tuples
title: Tuples
---

# Rust funciones que aceptan y devuelven tuplas

C no tiene ninguna noción de tuplas, pero el análogo más cercano es una estructura
simple. Tendrá que crear estructuras individuales para cada combinación única de
tipos. Aquí, creamos una estructura que representa dos enteros sin signo de 32 bits.

{% example src/lib.rs %}

```
extern crate libc;

use libc::uint32_t;
use std::convert::From;

// A Rust function that accepts a tuple
fn flip_things_around_rust(tup: (u32, u32)) -> (u32, u32) {
    let (a, b) = tup;
    (b+1, a-1)
}

// A struct that can be passed between C and Rust
#[repr(C)]
pub struct Tuple {
    x: uint32_t,
    y: uint32_t,
}

// Conversion functions
impl From<(u32, u32)> for Tuple {
    fn from(tup: (u32, u32)) -> Tuple {
        Tuple { x: tup.0, y: tup.1 }
    }
}

impl From<Tuple> for (u32, u32) {
    fn from(tup: Tuple) -> (u32, u32) {
        (tup.x, tup.y)
    }
}

// The exported C method
#[no_mangle]
pub extern fn flip_things_around(tup: Tuple) -> Tuple {
    flip_things_around_rust(tup.into()).into()
}

#[allow(dead_code)]
pub extern fn fix_linking_when_not_using_stdlib() { panic!() }
```

`# [repr(C)]` se usa para informar al compilador que debe organizar los campos de la
estructura como haría un compilador de C. Las dos implementaciones de conversión
usan [`std::convert::From`][From] para proporcionar una conversión ergonómica entre
la estructura y una tupla correspondiente.

[From]: http://doc.rust-lang.org/std/convert/trait.From.html

## C

{% example src/main.c %}

```
#include <stdio.h>
#include <stdint.h>

typedef struct {
  uint32_t x;
  uint32_t y;
} tuple_t;

extern tuple_t flip_things_around(tuple_t);

int main(void) {
  tuple_t initial = { .x = 10, .y = 20 };
  tuple_t new = flip_things_around(initial);
  printf("(%d,%d)\n", new.x, new.y);
  return 0;
}
```

Dado que nos adaptamos a las expresiones idiomáticas compatibles con C, la
implementación es *straight-forward*. Definimos una `struct` con campos que
coinciden con los tipos y el orden de la estructura Rust, luego creamos una
instancia y llamamos al método.

## Ruby

{% example src/main.rb %}

```
require 'ffi'

class Tuple < FFI::Struct
  layout :x, :uint32,
         :y, :uint32

  def to_s
    "(#{self[:x]},#{self[:y]})"
  end
end

module Tuples
  extend FFI::Library
  ffi_lib 'tuples'
  attach_function :flip_things_around, [Tuple.by_value], Tuple.by_value
end

tup = Tuple.new
tup[:x] = 10
tup[:y] = 20

puts Tuples.flip_things_around(tup)
```

Para reflejar la definición de la estructura, creamos una subclase de
`FFI::Struct` y usamos `layout` para especificar los nombres y tipos de campo.

Al adjuntar la función, usamos `by_value` para indicar que la estructura se pasará
directamente, sin la necesidad de indirección a través de punteros.

## Python

{% example src/main.py %}

```
#!/usr/bin/env python3

import sys, ctypes
from ctypes import c_uint32, Structure

class Tuple(Structure):
    _fields_ = [("x", c_uint32),
                ("y", c_uint32)]

    def __str__(self):
        return "({},{})".format(self.x, self.y)

prefix = {'win32': ''}.get(sys.platform, 'lib')
extension = {'darwin': '.dylib', 'win32': '.dll'}.get(sys.platform, '.so')
lib = ctypes.cdll.LoadLibrary(prefix + "tuples" + extension)

lib.flip_things_around.argtypes = (Tuple, )
lib.flip_things_around.restype = Tuple

tup = Tuple(10, 20)

print(lib.flip_things_around(tup))
```

Para reflejar la definición de la estructura, creamos una subclase de
`ctypes.Structure` y usamos `fields_` para especificar los nombres y tipos de campo.

## Haskell

Desafortunadamente, Haskell actualmente no admite pasar o devolver estructuras
arbitrarias. La indirección del puntero siempre se requiere.

## Node.js

{% example src/main.js %}

```
const ffi = require('ffi');
const struct = require('ref-struct');

const Tuple = struct({
  x: 'uint32',
  y: 'uint32',
});

const lib = ffi.Library('libtuples', {
  flip_things_around: [Tuple, [Tuple]],
});

const tup = new Tuple({x: 10, y: 20});
const result = lib.flip_things_around(tup);
console.log('(%d,%d)', result.x, result.y);
```

El paquete [`ref-struct`][ref-struct] nos permite construir tipos de estructuras que
se pueden pasar a las funciones FFI.

[ref-struct]: https://www.npmjs.com/package/ref-struct

## C\#

{% example src/main.cs %}

```
using System;
using System.Runtime.InteropServices;

[StructLayout(LayoutKind.Sequential)]
struct IntTuple {
    public uint x;
    public uint y;

    public static implicit operator Tuple<uint, uint>(IntTuple t)
    {
        return Tuple.Create(t.x, t.y);
    }

    public static implicit operator IntTuple(Tuple<uint, uint> t)
    {
        return new IntTuple { x = t.Item1, y = t.Item2 };
    }
};

class Tuples
{
    [DllImport("tuples")]
    private static extern IntTuple flip_things_around(IntTuple t);

    public static Tuple<uint, uint> FlipThingsAround(Tuple<uint, uint> t)
    {
        return flip_things_around(t);
    }

    static public void Main()
    {
        var tuple = Tuple.Create(10u, 20u);
        var newTuple = Tuples.FlipThingsAround(tuple);
        Console.WriteLine($"({newTuple.Item1},{newTuple.Item2})");
    }
}
```

Para reflejar la definición de la estructura de tuplas, creamos una `struct` usando
la propiedad `StructLayout` y definimos el diseño como secuencial. También
proporcionamos operadores de conversión implícitos para hacer que los tipos sean bastante fluidos.
