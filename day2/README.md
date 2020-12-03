# day2
_Clojure_ app build with [Leiningen](https://leiningen.org).
Calculates the number of valid passwords in a file.

## Sled rental password policy
> For example, suppose you have the following list:
```
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
```
> Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
>
> In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.

## Toboggan rental password policy
> Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter. Other occurrences of the letter are irrelevant for the purposes of policy enforcement.
>
> Given the same example list from above:
```
1-3 a: abcde is valid: position 1 contains a and position 3 does not.
1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
```


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
