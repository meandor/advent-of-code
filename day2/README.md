# day2
_Clojure_ app build with [Leiningen](https://leiningen.org).
Calculates the number of valid passwords in a file.

> For example, suppose you have the following list:
>
> 1-3 a: abcde
> 1-3 b: cdefg
> 2-9 c: ccccccccc
>
> Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
>
> In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.

## Run
```bash
lein run <path-to-input.txt>
```
There is an example input.txt in `resources/input.txt`

## Run tests
```bash
lein test
```

## Build jar
```bash
lein uberjar
```
This will create a `day2-<version>-standalone.jar` file in `/target/uberjar`
