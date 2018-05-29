---
layout: default
examples: ../examples/slice_arguments
title: Slice Arguments
---

# Rust funciones con argumentos *slice*

Los *slices* Rust integran el concepto de un puntero a un *chunk* de datos junto con
la cantidad de elementos. En C, las matrices están compuestas por las mismas piezas,
pero no existe un contenedor estándar que las mantenga unidas.

{% example src/lib.rs %}

```
extern crate libc;

use libc::{uint32_t, size_t};
use std::slice;

#[no_mangle]
pub extern fn sum_of_even(n: *const uint32_t, len: size_t) -> uint32_t {
    let numbers = unsafe {
        assert!(!n.is_null());

        slice::from_raw_parts(n, len as usize)
    };

    let sum =
        numbers.iter()
        .filter(|&v| v % 2 == 0)
        .fold(0, |acc, v| acc + v);
    sum as uint32_t
}
```

Convertir una matriz es un proceso de dos pasos:

1. Afirme que el puntero C no es `NULL` ya que las referencias a Rust nunca pueden
ser `NULL`.

2. Utilice [`from_raw_parts`][from_raw_parts] para convertir el puntero y la longitud en un *slice*. Esta es una operación insegura porque podemos *dereferencing* (*desreferenciar*) memoria no válida.

[from_raw_parts]: http://doc.rust-lang.org/std/slice/fn.from_raw_parts.html

## C

{% example src/main.c %}

```
#include <stdio.h>
#include <stdint.h>

extern uint32_t sum_of_even(const uint32_t *numbers, size_t length);

int main(void) {
  uint32_t numbers[6] = {1,2,3,4,5,6};
  uint32_t sum = sum_of_even(numbers, 6);
  printf("%d\n", sum);
  return 0;
}
```

La llamada desde C es *straight-forward*, ya que hemos hecho que el código Rust
coincida con las capacidades de C. La única complicación es asegurar que la cantidad
de elementos sea la misma entre la definición y la llamada a la función.

## Ruby

{% example src/main.rb %}

```
require 'ffi'

module SliceArgumentsFFI
  extend FFI::Library
  ffi_lib 'slice_arguments'
  attach_function :sum_of_even, [:pointer, :size_t], :uint32
end

class SliceArguments
  extend SliceArgumentsFFI

  def self.sum_of_even(numbers)
    buf = FFI::MemoryPointer.new(:uint32, numbers.size)
    buf.write_array_of_uint32(numbers)
    super(buf, numbers.size)
  end
end

puts SliceArguments.sum_of_even([1,2,3,4,5,6])
```

Llamar desde Ruby requiere más trabajo que los ejemplos anteriores. Esta vez, usamos
[`MemoryPointer`][MemoryPointer] para asignar espacio para almacenar nuestros
enteros. Una vez creado, copiamos los valores en él usando `write_array_of_uint32`.

[MemoryPointer]: https://github.com/ffi/ffi/wiki/Pointers#memorypointer

## Python

{% example src/main.py %}

```
#!/usr/bin/env python3

import sys, ctypes
from ctypes import POINTER, c_uint32, c_size_t

prefix = {'win32': ''}.get(sys.platform, 'lib')
extension = {'darwin': '.dylib', 'win32': '.dll'}.get(sys.platform, '.so')
lib = ctypes.cdll.LoadLibrary(prefix + "slice_arguments" + extension)

lib.sum_of_even.argtypes = (POINTER(c_uint32), c_size_t)
lib.sum_of_even.restype = ctypes.c_uint32

def sum_of_even(numbers):
    buf_type = c_uint32 * len(numbers)
    buf = buf_type(*numbers)
    return lib.sum_of_even(buf, len(numbers))

print(sum_of_even([1,2,3,4,5,6]))
```

Llamar desde Python requiere más trabajo que los ejemplos anteriores. Esta vez,
creamos un nuevo tipo para almacenar nuestros enteros e instanciar el tipo usando
los valores.

## Haskell

{% example src/main.hs %}

```
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Word (Word32)
import Foreign (Ptr)
import Foreign.Marshal.Array (withArrayLen)

foreign import ccall "sum_of_even"
  sum_of_even :: Ptr Word32 -> Word32 -> Word32

main :: IO ()
main = withArrayLen [1,2,3,4,5,6] $ \len arr ->
    print (sum_of_even arr (fromIntegral len))
```

Para este ejemplo, podemos usar la función `withArrayLen`, que toma una matriz
Haskell cuyos contenidos son `Storable` (es decir, secuencias serializables en bytes
que C puede entender) y produce una matriz empaquetada de esos valores, que luego
pasa, junto con con la longitud de la matriz, a una función de devolución de
llamada. En este caso, pasa la longitud de la matriz como tipo `Int`, que
convertimos en el tipo `CUInt` esperado utilizando la función `fromIntegral`.

## Node.js

{% example src/main.js %}

```
const ffi = require('ffi');
const ref = require('ref');
const array = require('ref-array');

const U32array = array(ref.types.uint32);

const lib = ffi.Library('libslice_arguments', {
  sum_of_even: ['uint32', [U32array, 'size_t']],
});

const numbers = new U32array([1, 2, 3, 4, 5, 6]);
console.log(lib.sum_of_even(numbers, numbers.length));
```

Necesitamos usar los paquetes [`ref`][ref] y [`ref-array`][ref-array] para envolver
los almacenamientos intermedios de memoria *node.js* en objetos tipo array que se
pueden manipular fácilmente desde JavaScript. El tipo `u32array` (construido usando
primitivas de `ref.types`) se puede usar luego en las firmas de función.

[ref]: https://www.npmjs.com/package/ref
[ref-array]: https://www.npmjs.com/package/ref-array

## C\#

{% example src/main.cs %}

```
using System;
using System.Runtime.InteropServices;

class SliceArguments
{
    [DllImport("slice_arguments")]
    private static extern uint sum_of_even(int[] n, UIntPtr len);

    public static uint SumOfEven(int[] n)
    {
        return sum_of_even(n, (UIntPtr)n.Length);
    }

    static public void Main()
    {
        var sum = SliceArguments.SumOfEven(new [] {1,2,3,4,5,6});
        Console.WriteLine(sum);
    }
}
```

Pasar una matriz es complicado ya que necesitamos pasar tanto un puntero a los datos
como la longitud de la matriz. A diferencia de los ejemplos anteriores, traemos la
función no-idiomática `snake_case` como un método privado. Luego podemos agregar un método público que envuelve el privado y proporciona la interfaz esperada.

El código C usa `size_t`, un tipo cuyo tamaño cambia según la plataforma. Para reflejar eso, usamos un `UIntPtr`.
