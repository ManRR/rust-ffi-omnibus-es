---
layout: default
examples: ../examples/string_arguments/
title: String Arguments
---

# Rust funciones con argumentos de *string*

Comencemos por algo un poco más complejo, aceptando *string* como argumentos.
En Rust, los *strings* se componen de un *slice* de `u8` y se garantiza que sean UTF-8 válidas, lo que permite bytes `NUL` en el interior de el *string*. En C, los *strings* son solo punteros a un `char` y terminan en un byte `NUL` (con el valor
entero `0`). Se necesita algo de trabajo para convertir estas dos representaciones.

{% example src/lib.rs %}

```
extern crate libc;

use libc::{c_char, uint32_t};
use std::ffi::CStr;
use std::str;

#[no_mangle]
pub extern fn how_many_characters(s: *const c_char) -> uint32_t {
    let c_str = unsafe {
        assert!(!s.is_null());

        CStr::from_ptr(s)
    };

    let r_str = c_str.to_str().unwrap();
    r_str.chars().count() as uint32_t
}
```

Obtener un *string slice* Rust (`&str`) requiere unos pocos pasos:

1. Tenemos que asegurarnos de que el puntero C no sea `NULL` ya que en Rust
 las referencias no pueden ser `NULL`.

2. Utilice [`std::ffi::CStr`][CStr] para envolver el puntero. `CStr` calculará la
 longitud del *string* en función de la terminación `NUL`. Esto requiere un bloque
 `unsafe`, ya que estaremos desreferenciando un *raw pointer*, que el compilador de
 Rust no puede verificar y cumple con todas las garantías de seguridad, por lo que el programador debe hacerlo en su lugar.

3. Asegúrese de que el *string* de C sea UTF-8 válido y conviértala en un *string slice* de Rust.

4. Usa el *string slice*.

En este ejemplo, simplemente cancelamos el programa si falla alguna de nuestras
condiciones previas. Cada caso de uso debe evaluar cuáles son los modos de falla
apropiados, pero fallar fuerte y temprano es una buena posición inicial.

[CStr]: http://doc.rust-lang.org/std/ffi/struct.CStr.html
[to_str]: https://doc.rust-lang.org/nightly/std/ffi/struct.CStr.html#method.to_str

## *Ownership* y *lifetimes*

En este ejemplo, el código Rust **no* *es el propietario del *string slice*, y el
compilador solo permitirá que el *string* viva mientras dure la instancia `CStr`. Depende del programador asegurarse de que este *lifetime* sea lo suficientemente corto.

## C

{% example src/main.c %}

```
#include <stdio.h>
#include <inttypes.h>

extern uint32_t how_many_characters(const char *str);

int main(void) {
  uint32_t count = how_many_characters("göes to élevên");
  printf("%d\n", count);
  return 0;
}
```

El código C declara que la función acepta un puntero a un *string*, ya que la
función Rust no lo modificará. A continuación, puede llamar a la función con una constante de *string* C normal.

## Ruby

{% example src/main.rb %}

```
# coding: utf-8
require 'ffi'

module StringArguments
  extend FFI::Library
  ffi_lib 'string_arguments'
  attach_function :how_many_characters, [:string], :uint32
end

puts StringArguments.how_many_characters("göes to élevên")
```

La gema FFI convierte automáticamente los *strings* de Ruby en el *strings* de C adecuado.

## Python

{% example src/main.py %}

```
#!/usr/bin/env python3
# coding: utf-8

import sys, ctypes
from ctypes import c_uint32, c_char_p

prefix = {'win32': ''}.get(sys.platform, 'lib')
extension = {'darwin': '.dylib', 'win32': '.dll'}.get(sys.platform, '.so')
lib = ctypes.cdll.LoadLibrary(prefix + "string_arguments" + extension)

lib.how_many_characters.argtypes = (c_char_p,)
lib.how_many_characters.restype = c_uint32

print(lib.how_many_characters("göes to élevên".encode('utf-8')))
```

Los *strings* de Python se deben codificar como UTF-8 para pasar a través de la
frontera de FFI.

## Haskell

{% example src/main.hs %}

```
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Word (Word32)
import Foreign.C.String (CString(..), newCString)

foreign import ccall "how_many_characters"
  how_many_characters :: CString -> Word32

main :: IO ()
main = do
  str <- newCString "göes to élevên"
  print (how_many_characters str)
```

El módulo `Foreign.C.String` tiene soporte para convertir la representación de *string* de Haskell a la representación de bytes empaquetados de C. Podemos crear uno con la función `newCString`, y luego pasar el valor `CString` a nuestra llamada externa.

## Node.js

{% example src/main.js %}

```
const ffi = require('ffi');

const lib = ffi.Library('libstring_arguments', {
  how_many_characters: ['uint32', ['string']],
});

console.log(lib.how_many_characters('göes to élevên'));
```

El paquete `ffi` convierte automáticamente *strings* de JavaScript a los *strings*
de C apropiados.

## C\#

{% example src/main.cs %}

```
using System;
using System.Runtime.InteropServices;

class StringArguments
{
    [DllImport("string_arguments", EntryPoint="how_many_characters")]
    public static extern uint HowManyCharacters(string s);

    static public void Main()
    {
        var count = StringArguments.HowManyCharacters("göes to élevên");
        Console.WriteLine(count);
    }
}
```

Los *strings* nativos se presentan automáticamente en *strings* compatibles con C.
