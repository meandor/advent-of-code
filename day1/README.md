# day 1
A [_Prolog_](https://www.swi-prolog.org/) app to find the two entries that sum to 2020.

## day 1.1
> Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
>
> For example, suppose your expense report contained the following:
```
1721
979
366
299
675
1456
```  
> In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

## day 1.2
> Find three numbers in your expense report that meet the same criteria.
> Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.

## Run
You can run it within the given docker image or within a prolog session:
```prolog
?- consult(main).
```
