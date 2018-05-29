---
layout: default
examples: ../examples/integers/
title: Integers
---

# Pasar y devolver enteros

Los enteros son el "hello world!" de FFI, ya que en general son mucho más fáciles de
atravesar la frontera. Vamos a crear una biblioteca que agregue dos números de
32 bits sin firmar.

{% example src/lib.rs %}

```
extern crate libc;
use libc::uint32_t;

#[no_mangle]
pub extern fn addition(a: uint32_t, b: uint32_t) -> uint32_t {
    a + b
}

#[allow(dead_code)]
pub extern fn fix_linking_when_not_using_stdlib() { panic!() }
```

Compile esto con `cargo build`, que producirá una biblioteca en `target/debug/`. El
nombre de archivo exacto depende de su plataforma:

| Platform | Pattern    |
|----------|------------|
| Windows  | *.dll      |
| OS X     | lib*.dylib |
| Linux    | lib*.so    |

## C

{% example src/main.c %}

```
#include <stdio.h>
#include <stdint.h>

extern uint32_t addition(uint32_t, uint32_t);

int main(void) {
  uint32_t sum = addition(1, 2);
  printf("%d\n", sum);
  return 0;
}
```

Comenzamos declarando una función `extern` con el argumento apropiado y los tipos de
retorno. Esto se puede compilar y vincular con la biblioteca de Rust usando
`gcc --std=c11 -o c-example src/main.c -L target/debug/ -lintegers`.

Como se indicó en la sección de conceptos básicos, esto se puede ejecutar en Mac OS
X y Linux con `LD_LIBRARY_PATH=target/debug/ ./c-example`, y en Windows copiando
`target\debug\integers.dll` en el directorio actual y ejecutando `.\c-example`.

## Ruby

{% example src/main.rb %}

```
require 'ffi'

module Integers
  extend FFI::Library
  ffi_lib 'integers'
  attach_function :addition, [:uint32, :uint32], :uint32
end

puts Integers.addition(1, 2)
```

Esto se puede ejecutar con `LD_LIBRARY_PATH=target/debug/ ruby./src/main.rb`

## Python

{% example src/main.py %}

```
#!/usr/bin/env python3

import sys, ctypes
from ctypes import c_uint32

prefix = {'win32': ''}.get(sys.platform, 'lib')
extension = {'darwin': '.dylib', 'win32': '.dll'}.get(sys.platform, '.so')
lib = ctypes.cdll.LoadLibrary(prefix + "integers" + extension)

lib.addition.argtypes = (c_uint32, c_uint32)
lib.addition.restype = c_uint32

print(lib.addition(1, 2))
```

Como se señaló en la sección de conceptos básicos, esto se puede ejecutar en Mac OS
X y Linux con `LD_LIBRARY_PATH=target/debug /python src/ main.py`, y en Windows
copiando `target\debug\integers.dll` a al directorio actual y ejecutando
`py src\main.py`.

## Haskell

{% example src/main.hs %}

```
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Word (Word32)

foreign import ccall "addition"
  addition :: Word32 -> Word32 -> Word32

main :: IO ()
main = print (addition 1 2)
```

Tenemos que habilitar la extensión del lenguaje `ForeignFunctionInterface` e
importar los tipos de bajo nivel relevantes antes de que podamos incluir una
declaración `foreign import`. Esto incluye la convención de llamada (`ccall`), el
nombre del símbolo (`"addition"`), el nombre de Haskell correspondiente (`addition`)
y el tipo de la función. Esta función es efectivamente pura, por lo que no incluimos
`IO` en el tipo, pero una función observablemente impura querría devolver un valor
`IO` para indicar que tiene efectos secundarios.

Esto se puede compilar usando
`ghc src/main.hs target/debug/libintegers.so -o haskell-example`.

## Node.js

{% example src/main.js %}

```
const ffi = require('ffi');

const lib = ffi.Library('libintegers', {
  addition: ['uint32', ['uint32', 'uint32']],
});

console.log(lib.addition(1, 2));
```

La función `Library` especifica el nombre de una biblioteca dinámica para vincular,
junto con una lista de firmas de funciones exportadas (en forma de
`function_name: [return_type, [argument_types]]`). Estas funciones están disponibles
como métodos del objeto devuelto por `Library`.

Esto se puede ejecutar con `LD_LIBRARY_PATH=target/debug node src/main.js`.

## C\#

{% example src/main.cs %}

```
using System;
using System.Runtime.InteropServices;

class Integers
{
    [DllImport("integers", EntryPoint="addition")]
    public static extern uint Addition(uint a, uint b);

    static public void Main()
    {
        var sum = Integers.Addition(1, 2);
        Console.WriteLine(sum);
    }
}
```

Usamos la funcionalidad *Platform Invoke* para acceder a funciones en una biblioteca
dinámica. El atributo `DllImport` enumera el nombre de la biblioteca en la que se
puede encontrar la función. Estas funciones están disponibles como métodos estáticos
de la clase. Para cumplir con los estándares de nomenclatura de C #, utilizamos la
propiedad `EntryPoint` para usar un nombre en mayúscula para la función expuesta.

Esto se puede compilar con `mcs -out:csharp-example src/main.cs` y se ejecuta con
`LD_LIBRARY_PATH=target/debug mono csharp-example`.
