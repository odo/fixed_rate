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
2> Args = [[E]||E<-lists:seq(1, 10)].
[[1],[2],[3],[4],[5],[6],[7],"\b","\t","\n"]
3> fixed_rate:run(Fun, Args, 100).
1
2
3
4
5
6
7
8
9
10
10 samples took 0.101313 s.
Rate: 98.70401626642187 ops
{0.101313,[10,9,8,7,6,5,4,3,2,1]}
```

By default it will wait after every single call. If that seems too wasteful you can provide an extra argument specifying the number of calls which are executed as fast as possible before waiting.

You can also parallelize tasks, say with 10 processes:

```
1> fixed_rate:run_parallel(10, fun(E) -> E end, lists:seq(1, 1000), 100).
100 samples took 10.000564 s.
Rate: 9.999436031807805 ops
100 samples took 10.000578 s.
Rate: 9.999422033406468 ops
100 samples took 10.000581 s.
Rate: 9.999419033754139 ops
100 samples took 10.000584 s.
Rate: 9.999416034103609 ops
100 samples took 10.000587 s.
Rate: 9.999413034454879 ops
100 samples took 10.000589 s.
Rate: 9.999411034690057 ops
100 samples took 10.000619 s.
Rate: 9.999381038313729 ops
100 samples took 10.000619 s.
Rate: 9.999381038313729 ops
100 samples took 10.000621 s.
Rate: 9.999379038561704 ops
100 samples took 10.000624 s.
Rate: 9.99937603893517 ops

1000 samples took 10.000624 s.
Rate: 99.9937603893517 ops
{10.000624,
 [[8,18,28,38,48,58,68,78,88,98,108,118,128,138,148,158,168,
   178,188,198,208,218,228,238,248,258|...],
  [7,17,27,37,47,57,67,77,87,97,107,117,127,137,147,157,167,
   177,187,197,207,217,227,237,247|...],
  [9,19,29,39,49,59,69,79,89,99,109,119,129,139,149,159,169,
   179,189,199,209,219,229,239|...],
  [10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,
   180,190,200,210,220,230|...],
  [1,11,21,31,41,51,61,71,81,91,101,111,121,131,141,151,161,
   171,181,191,201,211|...],
  [2,12,22,32,42,52,62,72,82,92,102,112,122,132,142,152,162,
   172,182,192,202|...],
  [3,13,23,33,43,53,63,73,83,93,103,113,123,133,143,153,163,
   173,183,193|...],
  [4,14,24,34,44,54,64,74,84,94,104,114,124,134,144,154,164,
   174,184|...],
  [5,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,
   175|...],
  [6,16,26,36,46,56,66,76,86,96,106,116,126,136,146,156,166|...]]}
```

The times for all processes along with the final result will be reported, so outliers can be spotted.
