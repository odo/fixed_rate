fixed_rate
=====


fixed_rate is an Erlang library that lets you execute funs at a constant rate.

Building
-----

```
git clone git://github.com/odo/fixed_rate.git
cd fixed_rate
./rebar compile
```

Usage
-----
Call `run(Fun, Args, Rate)` with a target rate (ops/Hz).

`erl -pz ./ebin`

```
1> Fun = fun(E) -> io:format("~p\n", [E]), E end.
#Fun<erl_eval.6.17052888>
2> Args = lists:seq(1, 10).
[1,2,3,4,5,6,7,8,9,10]
3> fixed_rate:run(Fun, Args, 100).
10
9
8
7
6
5
4
3
2
1
10 samples took 0.10123 s.
Rate: 98.78494517435543
{98.78494517435543,[1,2,3,4,5,6,7,8,9,10]}
```

By default it will wait after every single call. If that seems too wasteful you can provide an extra argument specifying the number of calls which are executed as fast as possible before waiting.