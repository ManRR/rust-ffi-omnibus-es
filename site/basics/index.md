---
layout: default
examples: ../examples/integers/
title: Basics
---

# Lo esencial

Se supone que está familiarizado con los conceptos básicos de Rust
y el lenguaje desde el que llamará. Debería haber leído la
[documentación oficial de FFI][oficial], pero aquí se tratarán
algunos conceptos básicos.

## Rust

Todos los ejemplos de Rust usarán [Cargo] y la [libc crate][libc]. Cada ejemplo tiene el siguiente texto estándar en `Cargo.toml`:

{% example Cargo.toml %}

```rust
[dependencies]
libc = "*"

[lib]
crate-type = ["cdylib"]
```

`crate-type = ["cdylib"]` crea una biblioteca enlazada dinámicamente.
Consulte la documentación de Cargo para
[bibliotecas dinámicas o estáticas][dyn-stat] para obtener más información.

`cdylib` fue [introducido en RFC 1510][rfc1510] y mejora el `dylib`
existente con un tamaño de archivo reducido y menos símbolos exportados.
Fue [implementado en Rust 1.10][rust-1.10]; si está utilizando versiones anteriores,
se recomienda que actualice, pero también puede usar `dylib` con mínimos efectos
negativos.

Algunos ejemplos son tan mínimos que no usan ninguna característica de la biblioteca
estándar de Rust. Esto desencadena un [problema conocido][rust-18807] y el enlace
fallará. La única solución consiste en incluir una función exportada pero no
utilizada que utiliza algo de la biblioteca estándar. Estas funciones se llaman
`fix_linking_when_not_using_stdlib` y se pueden eliminar de forma segura para
cualquier proyecto más grande.

## C

Todos los ejemplos C se compilarán utilizando el estándar C11.

## Ruby

Todos los ejemplos de Ruby usarán Ruby 2.4 y la [gema FFI][gem].

## Python

Todos los ejemplos de Python usarán Python 3.4 y la [biblioteca ctypes][ctypes].

## Haskell

Todos los ejemplos de Haskell usarán GHC 7.6 con la extensión de lenguaje `ForeignFunctionInterface` y solo la biblioteca `base` que viene con GHC.

## Node.js

Todos los ejemplos de Node.js usarán Node.js 8.7 y el [paquete ffi][node-ffi].

## C\#

Todos los ejemplos de C# compilarán con Mono 5.10. Se supone que este código
funcionará con los marcos CLR de Microsoft, pero esto no se ha probado.

## Ejecución de ejemplos

Al ejecutar ejemplos, debe asegurarse de que el sistema pueda localizar
la biblioteca dinámica de Rust.

Con la mayoría de las shells en Mac OS X y Linux, esto se puede hacer prefijando
comandos con `LD_LIBRARY_PATH=target/debug`. Por ejemplo, para ejecutar un ejemplo
de Python, puede usar `LD_LIBRARY_PATH=target/debug python src/main.py` del
directorio de ejemplo.

En Windows, el curso de acción más simple es copiar la biblioteca dinámica
compilada en el directorio de trabajo actual antes de ejecutar los ejemplos. Solo
necesita el archivo `.dll`. También tenga en cuenta que al ejecutar ejemplos de
Python, puede usar `py` en lugar de `python`, especialmente si tiene instaladas
varias versiones de Python.

[official]: https://doc.rust-lang.org/book/ffi.html
[Cargo]: https://crates.io/
[libc]: http://doc.rust-lang.org/libc/libc/index.html
[dyn-stat]: http://doc.crates.io/manifest.html#building-dynamic-or-static-libraries
[rfc1510]: https://github.com/rust-lang/rfcs/blob/master/text/1510-cdylib.md
[rust-1.10]: https://blog.rust-lang.org/2016/07/07/Rust-1.10.html
[rust-18807]: https://github.com/rust-lang/rust/issues/18807
[gem]: https://github.com/ffi/ffi
[ctypes]: https://docs.python.org/3/library/ctypes.html
[node-ffi]: https://www.npmjs.com/package/node-ffi
