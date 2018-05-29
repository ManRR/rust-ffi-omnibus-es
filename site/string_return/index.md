---
layout: default
examples: ../examples/string_return/
title: String Return Values
---

# Funciones de Rust que devuelven *allocated strings*

Devolver un *allocated string* a través de FFI es complicado por la misma razón que
[devolver un objeto es][objects]: el asignador de Rust puede ser diferente del
asignador en el otro lado de la frontera del FFI. También tiene las mismas
restricciones para *strings* terminadas en NUL que [pasando un argumento de
**strings*][string-arguments].

{% example src/lib.rs %}

```
extern crate libc;

use libc::{c_char, uint8_t};
use std::ffi::CString;
use std::iter;

#[no_mangle]
pub extern fn theme_song_generate(length: uint8_t) -> *mut c_char {
    let mut song = String::from("💣 ");
    song.extend(iter::repeat("na ").take(length as usize));
    song.push_str("Batman! 💣");

    let c_str_song = CString::new(song).unwrap();
    c_str_song.into_raw()
}

#[no_mangle]
pub extern fn theme_song_free(s: *mut c_char) {
    unsafe {
        if s.is_null() { return }
        CString::from_raw(s)
    };
}
```

Aquí usamos un par de métodos [`into_raw`][into_raw] y [`from_raw`][from_raw]. Estos
convierten un `CString` en un puntero sin formato que puede pasar a través de la
frontera de FFI. El *Ownership* del *string* se transfiere a la persona que llama,
pero la persona que llama debe devolver el *string* a Rust para desasignar
correctamente la memoria.

[objects]: ../objects
[string-arguments]: ../string_arguments
[into_raw]: https://doc.rust-lang.org/std/ffi/struct.CString.html#method.into_raw
[from_raw]: https://doc.rust-lang.org/std/ffi/struct.CString.html#method.from_raw

## C

{% example src/main.c %}

```
#include <stdio.h>
#include <stdint.h>

extern char *
theme_song_generate(uint8_t length);

extern void
theme_song_free(char *);

int main(void) {
  char *song = theme_song_generate(5);
  printf("%s\n", song);
  theme_song_free(song);
}
```

No hay nada muy interesante para la versión C: el `char *` se devuelve, se puede
imprimir y luego se transfiere para liberarlo.

## Ruby

{% example src/main.rb %}

```
require 'ffi'

class ThemeSong < FFI::AutoPointer
  def self.release(ptr)
    Binding.free(ptr)
  end

  def to_s
    @str ||= self.read_string.force_encoding('UTF-8')
  end

  module Binding
    extend FFI::Library
    ffi_lib 'string_return'

    attach_function :generate, :theme_song_generate,
                    [:uint8], ThemeSong
    attach_function :free, :theme_song_free,
                    [ThemeSong], :void
  end
end

puts ThemeSong::Binding.generate(5)
```

Como el *string* está *asignada* (*allocated*), debemos asegurarnos de desasignarla
cuando se sale del alcance. Como un [object][objects], we subclass `FFI::
AutoPointer` para liberar automáticamente el puntero por nosotros.

Definimos `to_s` para convertir *lazily* el *string* sin formato a un *string* de
Ruby usando la codificación UTF-8 y memorizar el resultado. Cualquier *string*
generada por Rust será un UTF-8 válido.

## Python

{% example src/main.py %}

```
#!/usr/bin/env python3

import sys, ctypes
from ctypes import c_void_p, c_uint8

prefix = {'win32': ''}.get(sys.platform, 'lib')
extension = {'darwin': '.dylib', 'win32': '.dll'}.get(sys.platform, '.so')
lib = ctypes.cdll.LoadLibrary(prefix + "string_return" + extension)

lib.theme_song_generate.argtypes = (c_uint8, )
lib.theme_song_generate.restype = c_void_p

lib.theme_song_free.argtypes = (c_void_p, )

def themeSongGenerate(count):
    ptr = lib.theme_song_generate(count)
    try:
        return ctypes.cast(ptr, ctypes.c_char_p).value.decode('utf-8')
    finally:
        lib.theme_song_free(ptr)

print(themeSongGenerate(5))
```

Debemos usar `c_void_p` en lugar de `c_char_p` ya que un valor de retorno de tipo
`c_char_p` se convertirá automáticamente en un *string* de Python. Este *string*
sería liberado incorrectamente por Python, en lugar de por Rust.

Lanzamos el `c_void_p` a un `c_char_p`, tomamos el valor y codificamos los bytes sin
formato como un *string* UTF-8.

## Haskell

{% example src/main.hs %}

```
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Word (Word8)
import Foreign.Ptr (nullPtr)
import Foreign.C.String (CString(..), peekCString)

foreign import ccall unsafe "theme_song_generate"
  theme_song_generate :: Word8 -> IO (CString)

foreign import ccall unsafe "theme_song_free"
  theme_song_free :: CString -> IO ()

createThemeSong :: Word8 -> IO (Maybe (String))
createThemeSong len = do
  ptr <- theme_song_generate len
  if ptr /= nullPtr
    then do
      str <- peekCString ptr
      theme_song_free ptr
      return $ Just str
    else
      return Nothing

main :: IO ()
main = do
  song <- createThemeSong 5
  case song of
    Nothing -> putStrLn "Unable to create theme song"
    Just str -> putStrLn str
```

Después de llamar al método FFI, verificamos si el *string* es `NULL`. Si no, lo
convertimos en un *string* de Haskell usando `peekCString` y liberamos el
*string* de Rust.

## Node.js

{% example src/main.js %}

```
const ffi = require('ffi');

const lib = ffi.Library('libstring_return', {
  theme_song_generate: ['char *', ['uint8']],
  theme_song_free: ['void', ['char *']],
});

function themeSongGenerate(len) {
  const songPtr = lib.theme_song_generate(len);
  try {
    return songPtr.readCString();
  } finally {
    lib.theme_song_free(songPtr);
  }
}

console.log(themeSongGenerate(5));
```

El *string* se devuelve como `char *`, que podemos convertir a un *string* de JavaScript llamando `readCString` antes de devolverlo para liberarla.

## C\#

{% example src/main.cs %}

```
using System;
using System.Runtime.InteropServices;
using System.Text;

internal class Native
{
    [DllImport("string_return")]
    internal static extern ThemeSongHandle theme_song_generate(byte length);
    [DllImport("string_return")]
    internal static extern void theme_song_free(IntPtr song);
}

internal class ThemeSongHandle : SafeHandle
{
    public ThemeSongHandle() : base(IntPtr.Zero, true) {}

    public override bool IsInvalid
    {
        get { return false; }
    }

    public string AsString()
    {
        int len = 0;
        while (Marshal.ReadByte(handle, len) != 0) { ++len; }
        byte[] buffer = new byte[len];
        Marshal.Copy(handle, buffer, 0, buffer.Length);
        return Encoding.UTF8.GetString(buffer);
    }

    protected override bool ReleaseHandle()
    {
        Native.theme_song_free(handle);
        return true;
    }
}

public class ThemeSong : IDisposable
{
    private ThemeSongHandle song;
    private string songString;

    public ThemeSong(byte length)
    {
        song = Native.theme_song_generate(length);
    }

    public override string ToString()
    {
        if (songString == null) {
            songString = song.AsString();
        }
        return songString;
    }

    public void Dispose()
    {
        song.Dispose();
    }

    static public void Main()
    {
          var song = new ThemeSong(5);
          Console.WriteLine("{0}", song);
    }
}
```

Seguimos un patrón similar al ejemplo del objeto: el *string* de Rust está contenido
dentro de una subclase de `SafeHandle` y una clase contenedora `ThemeSong` asegura
que el identificador se elimine correctamente.

Desafortunadamente, no hay una manera fácil de leer el puntero como un *string*
UTF-8. C\# tiene casos para *string* ANSI y para *string* "Unicode" (realmente UCS-2
, pero nada para UTF-8. Necesitamos escribir eso nosotros mismos.
