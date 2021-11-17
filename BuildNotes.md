# Build Notes

## HsOpenSSL

Ubuntu

```shell
sudo apt-get install libssl-dev
```

Msys64 Mingw64 shell

```
pacman -Sy mingw-w64-x86_64-openssl
cabal get HsOpenSSL-0.11.4.14
cd HsOpenSSL-0.11.4.14
cabal install --user --extra-include-dirs=/mingw64/include --extra-lib-dirs=/mingw64/lib --extra-lib-dirs=/mingw64/bin
```

extra-lib path is absolute to the msys64/mingw64 in the cabal file

https://slproweb.com/products/Win32OpenSSL.html
