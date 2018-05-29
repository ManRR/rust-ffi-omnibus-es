---
layout: default
examples: ../examples/objects
title: Objects
---

# Usar objetos Rust desde otros lenguajes

Vamos a crear un objeto Rust que nos dirá cuántas personas viven en cada código
postal de EE. UU. Queremos poder utilizar esta lógica en otros lenguajes, pero solo
necesitamos pasar primitivas simples como enteros o *strings* a través de la
frontera de FFI. El objeto tendrá métodos mutables e inmutables. Debido a que no
podemos mirar dentro del objeto, esto se conoce como un *opaque object* o un
*opaque pointer*

{% example src/lib.rs %}

```
extern crate libc;

use libc::{c_char, uint32_t};
use std::str;
use std::collections::HashMap;
use std::ffi::CStr;

pub struct ZipCodeDatabase {
    population: HashMap<String, u32>,
}

impl ZipCodeDatabase {
    fn new() -> ZipCodeDatabase {
        ZipCodeDatabase {
            population: HashMap::new(),
        }
    }

    fn populate(&mut self) {
        for i in 0..100000 {
            let zip = format!("{:05}", i);
            self.population.insert(zip, i);
        }
    }

    fn population_of(&self, zip: &str) -> u32 {
        self.population.get(zip).cloned().unwrap_or(0)
    }
}

#[no_mangle]
pub extern fn zip_code_database_new() -> *mut ZipCodeDatabase {
    Box::into_raw(Box::new(ZipCodeDatabase::new()))
}

#[no_mangle]
pub extern fn zip_code_database_free(ptr: *mut ZipCodeDatabase) {
    if ptr.is_null() { return }
    unsafe { Box::from_raw(ptr); }
}

#[no_mangle]
pub extern fn zip_code_database_populate(ptr: *mut ZipCodeDatabase) {
    let database = unsafe {
        assert!(!ptr.is_null());
        &mut *ptr
    };
    database.populate();
}

#[no_mangle]
pub extern fn zip_code_database_population_of(ptr: *const ZipCodeDatabase, zip: *const c_char) -> uint32_t {
    let database = unsafe {
        assert!(!ptr.is_null());
        &*ptr
    };
    let zip = unsafe {
        assert!(!zip.is_null());
        CStr::from_ptr(zip)
    };
    let zip_str = zip.to_str().unwrap();
    database.population_of(zip_str)
}
```

La `struct` se define de forma normal para Rust. Se crea una función `extern` para
cada función del objeto. C no tiene un concepto de espacio de nombres incorporado,
por lo que es normal prefijar cada función con un nombre de paquete y/o un nombre
de tipo. Para este ejemplo, usamos `zip_code_database`. Siguiendo las convenciones
normales de C, siempre se proporciona un puntero al objeto como primer argumento.

Para crear una nueva instancia de objeto, colocamos el resultado del constructor del
objeto. Esto coloca la estructura en el montículo donde tendrá una dirección de
memoria estable. Esta dirección se convierte en un *raw pointer* usando
[`Box::into_raw`][into_raw].

Este puntero apunta a la memoria asignada por Rust; la memoria asignada por Rust
**debe** ser desasignada por Rust. Usamos [`Box::from_raw`][from_raw] para convertir
el puntero de nuevo a `Box<ZipCodeDatabase>` cuando el objeto se va a liberar. A
diferencia de otras funciones, *hacemos* que se pase `NULL`, pero simplemente no
hacemos nada en ese caso. Esto es una buena noticia para los programadores clientes.

Para crear una referencia desde un *raw pointer*, puede usar la sintaxis breve
`&*`, que indica que el puntero debe desreferenciarse y luego volver a
referenciarse. Crear una referencia mutable es similar, pero usa `&mut *`. Al igual
que otros punteros, debe asegurarse de que el puntero no sea `NULL`.

Tenga en cuenta que un `*const T` se puede convertir libremente desde y hacia un
`*mut T` y que nada impide que el código del cliente nunca llame a la función de
desasignación, o que lo llame más de una vez. Las garantías de gestión y seguridad
de la memoria están completamente en manos del programador.

[into_raw]: https://doc.rust-lang.org/std/boxed/struct.Box.html#method.into_raw
[from_raw]: https://doc.rust-lang.org/std/boxed/struct.Box.html#method.from_raw

## C

{% example src/main.c %}

```
#include <stdio.h>
#include <stdint.h>

typedef struct zip_code_database_S zip_code_database_t;

extern zip_code_database_t *
zip_code_database_new(void);

extern void
zip_code_database_free(zip_code_database_t *);

extern void
zip_code_database_populate(zip_code_database_t *);

extern uint32_t
zip_code_database_population_of(const zip_code_database_t *, const char *zip);

int main(void) {
  zip_code_database_t *database = zip_code_database_new();

  zip_code_database_populate(database);
  uint32_t pop1 = zip_code_database_population_of(database, "90210");
  uint32_t pop2 = zip_code_database_population_of(database, "20500");

  zip_code_database_free(database);

  printf("%d\n", pop1 - pop2);
}
```

Se crea una estructura ficticia para proporcionar una pequeña cantidad de *seguridad
de tipo* (*type-safety*).

El modificador `const` se usa en funciones donde sea apropiado, aunque la corrección
de const es mucho más fluida en C que en Rust.

## Ruby

{% example src/main.rb %}

```
require 'ffi'

class ZipCodeDatabase < FFI::AutoPointer
  def self.release(ptr)
    Binding.free(ptr)
  end

  def populate
    Binding.populate(self)
  end

  def population_of(zip)
    Binding.population_of(self, zip)
  end

  module Binding
    extend FFI::Library
    ffi_lib 'objects'

    attach_function :new, :zip_code_database_new,
                    [], ZipCodeDatabase
    attach_function :free, :zip_code_database_free,
                    [ZipCodeDatabase], :void
    attach_function :populate, :zip_code_database_populate,
                    [ZipCodeDatabase], :void
    attach_function :population_of, :zip_code_database_population_of,
                    [ZipCodeDatabase, :string], :uint32
  end
end

database = ZipCodeDatabase::Binding.new

database.populate
pop1 = database.population_of("90210")
pop2 = database.population_of("20500")

puts pop1 - pop2
```

Para envolver las *raw functions*, creamos una clase pequeña que hereda de
[`AutoPointer`][AutoPointer]. `AutoPointer` se asegurará de que el recurso
subyacente se libere cuando se libera el objeto. Para hacer esto, el usuario debe
definir el método `self.release`.

Lamentablemente, como heredamos de `AutoPointer`, no podemos redefinir el
inicializador. Para agrupar mejor los conceptos, vinculamos los métodos FFI en un
módulo anidado. Proporcionamos nombres más cortos para los métodos enlazados, lo que
permite al cliente simplemente llamar a `ZipCodeDatabase::Binding.new`.

[AutoPointer]: http://www.rubydoc.info/github/ffi/ffi/FFI/AutoPointer

## Python

{% example src/main.py %}

```
#!/usr/bin/env python3

import sys, ctypes
from ctypes import c_char_p, c_uint32, Structure, POINTER

class ZipCodeDatabaseS(Structure):
    pass

prefix = {'win32': ''}.get(sys.platform, 'lib')
extension = {'darwin': '.dylib', 'win32': '.dll'}.get(sys.platform, '.so')
lib = ctypes.cdll.LoadLibrary(prefix + "objects" + extension)

lib.zip_code_database_new.restype = POINTER(ZipCodeDatabaseS)

lib.zip_code_database_free.argtypes = (POINTER(ZipCodeDatabaseS), )

lib.zip_code_database_populate.argtypes = (POINTER(ZipCodeDatabaseS), )

lib.zip_code_database_population_of.argtypes = (POINTER(ZipCodeDatabaseS), c_char_p)
lib.zip_code_database_population_of.restype = c_uint32

class ZipCodeDatabase:
    def __init__(self):
        self.obj = lib.zip_code_database_new()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        lib.zip_code_database_free(self.obj)

    def populate(self):
        lib.zip_code_database_populate(self.obj)

    def population_of(self, zip):
        return lib.zip_code_database_population_of(self.obj, zip.encode('utf-8'))

with ZipCodeDatabase() as database:
    database.populate()
    pop1 = database.population_of("90210")
    pop2 = database.population_of("20500")
    print(pop1 - pop2)
```

Creamos una estructura vacía para representar nuestro tipo. Esto solo se usará junto
con el método `POINTER`, que crea un nuevo tipo como un puntero a uno existente.

Para garantizar que la memoria se limpia correctamente, utilizamos un
*context manager*. Esto está vinculado a nuestra clase a través de los métodos
`__enter__` y `__exit__`. Usamos la instrucción `with` para comenzar un nuevo
contexto. Cuando finalice el contexto, se llamará automáticamente al método
`__exit__` para evitar el *memory leak*

## Haskell

{% example src/main.hs %}

```
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Word (Word32)
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String (CString(..), newCString)

data ZipCodeDatabase

foreign import ccall unsafe "zip_code_database_new"
  zip_code_database_new :: IO (Ptr ZipCodeDatabase)

foreign import ccall unsafe "&zip_code_database_free"
  zip_code_database_free :: FunPtr (Ptr ZipCodeDatabase -> IO ())

foreign import ccall unsafe "zip_code_database_populate"
  zip_code_database_populate :: Ptr ZipCodeDatabase -> IO ()

foreign import ccall unsafe "zip_code_database_population_of"
  zip_code_database_population_of :: Ptr ZipCodeDatabase -> CString -> Word32

createDatabase :: IO (Maybe (ForeignPtr ZipCodeDatabase))
createDatabase = do
  ptr <- zip_code_database_new
  if ptr /= nullPtr
    then do
      foreignPtr <- newForeignPtr zip_code_database_free ptr
      return $ Just foreignPtr
    else
      return Nothing

populate = zip_code_database_populate

populationOf :: Ptr ZipCodeDatabase -> String -> IO (Word32)
populationOf db zip = do
  zip_str <- newCString zip
  return $ zip_code_database_population_of db zip_str

main :: IO ()
main = do
  db <- createDatabase
  case db of
    Nothing -> putStrLn "Unable to create database"
    Just ptr -> withForeignPtr ptr $ \database -> do
        populate database
        pop1 <- populationOf database "90210"
        pop2 <- populationOf database "20500"
        print (pop1 - pop2)
```

Comenzamos definiendo un tipo vacío para referirnos al objeto opaco. Al definir las
funciones importadas, usamos el constructor de tipo `Ptr` con este nuevo tipo como
el tipo del puntero devuelto por Rust. También usamos `IO` como asignar, liberar y
poblar el objeto todas las funciones con efectos secundarios.

Como la asignación teóricamente puede fallar, buscamos `NULL` y devolvemos `Maybe`
del constructor. Es probable que esto sea excesivo, ya que ahora Rust aborta el
proceso cuando falla el asignador.

Para garantizar que la memoria asignada se libera automáticamente, usamos el tipo
`ForeignPtr`. Esto toma un `Ptr` *raw* y una función para llamar cuando el puntero
envuelto se desasigna.

Al usar el puntero envuelto, `withForeignPtr` se usa para desenvolverlo antes de
devolverlo a las funciones FFI.

## Node.js

{% example src/main.js %}

```
const ffi = require('ffi');

const lib = ffi.Library('libobjects', {
  zip_code_database_new: ['pointer', []],
  zip_code_database_free: ['void', ['pointer']],
  zip_code_database_populate: ['void', ['pointer']],
  zip_code_database_population_of: ['uint32', ['pointer', 'string']],
});

const ZipCodeDatabase = function() {
  this.ptr = lib.zip_code_database_new();
};

ZipCodeDatabase.prototype.free = function() {
  lib.zip_code_database_free(this.ptr);
};

ZipCodeDatabase.prototype.populate = function() {
  lib.zip_code_database_populate(this.ptr);
};

ZipCodeDatabase.prototype.populationOf = function(zip) {
  return lib.zip_code_database_population_of(this.ptr, zip);
};

const database = new ZipCodeDatabase();
try {
  database.populate();
  const pop1 = database.populationOf('90210');
  const pop2 = database.populationOf('20500');
  console.log(pop1 - pop2);
} finally {
  database.free();
}
```

Al importar las funciones, simplemente declaramos que se devuelve o acepta un tipo
de `pointer`.

Para hacer el acceso a las funciones más limpias, creamos una clase simple que
mantiene el puntero para nosotros y lss abstracciones que pasan a las funciones de
nivel inferior. Esto también nos da la oportunidad de cambiar el nombre de las
funciones con el idiomático JavaScript camel-case.

Para garantizar que los recursos se limpien, usamos un bloque `try` y llamamos al
método de desasignación en el bloque `finally`.

## C\#

{% example src/main.cs %}

```
using System;
using System.Runtime.InteropServices;

internal class Native
{
    [DllImport("objects")]
    internal static extern ZipCodeDatabaseHandle zip_code_database_new();
    [DllImport("objects")]
    internal static extern void zip_code_database_free(IntPtr db);
    [DllImport("objects")]
    internal static extern void zip_code_database_populate(ZipCodeDatabaseHandle db);
    [DllImport("objects")]
    internal static extern uint zip_code_database_population_of(ZipCodeDatabaseHandle db, string zip);
}

internal class ZipCodeDatabaseHandle : SafeHandle
{
    public ZipCodeDatabaseHandle() : base(IntPtr.Zero, true) {}

    public override bool IsInvalid
    {
        get { return false; }
    }

    protected override bool ReleaseHandle()
    {
        Native.zip_code_database_free(handle);
        return true;
    }
}

public class ZipCodeDatabase : IDisposable
{
    private ZipCodeDatabaseHandle db;

    public ZipCodeDatabase()
    {
        db = Native.zip_code_database_new();
    }

    public void Populate()
    {
        Native.zip_code_database_populate(db);
    }

    public uint PopulationOf(string zip)
    {
        return Native.zip_code_database_population_of(db, zip);
    }

    public void Dispose()
    {
        db.Dispose();
    }

    static public void Main()
    {
          var db = new ZipCodeDatabase();
          db.Populate();

          var pop1 = db.PopulationOf("90210");
          var pop2 = db.PopulationOf("20500");

          Console.WriteLine("{0}", pop1 - pop2);
    }
}
```

Como las responsabilidades para llamar a las funciones nativas van a estar más
extendidas, creamos una clase `Native` para contener todas las definiciones.

Para garantizar que la memoria asignada se libere automáticamente, creamos una
subclase de la clase [`SafeHandle`][SafeHandle]. Esto requiere implementar
`IsInvalid` y `ReleaseHandle`. Dado que nuestra función Rust acepta liberar un
puntero `NULL`, podemos decir que cada puntero es válido.

Podemos usar nuestra envoltura segura `ZipCodeDatabaseHandle` como el tipo de las
funciones FFI excepto para la función *free*. El puntero real se organizará
automáticamente hacia y desde el contenedor.

También permitimos que `ZipCodeDatabase` participe en el protocolo `IDisposable`,
reenviando al contenedor seguro.

[SafeHandle]: https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.safehandle(v=vs.110).aspx
